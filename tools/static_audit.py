from pathlib import Path
import csv, hashlib, json, re, shutil, subprocess, sys

pkg = Path(sys.argv[1] if len(sys.argv)>1 else '.').resolve()
Rdir = pkg/'R'

def exported_blocks():
    out={}
    for path in sorted(Rdir.glob('*.R')):
        lines=path.read_text(encoding='utf-8').splitlines()
        i=0
        while i<len(lines):
            if lines[i].startswith("#'"):
                block=[]; start=i+1
                while i<len(lines) and lines[i].startswith("#'"):
                    block.append(lines[i][2:].lstrip()); i+=1
                j=i
                while j<len(lines) and not lines[j].strip(): j+=1
                m=re.match(r'([A-Za-z][A-Za-z0-9_.]*)\s*<-\s*function\s*\(', lines[j] if j<len(lines) else '')
                if m and any(x.strip()=='@export' for x in block):
                    name=m.group(1)
                    examples=sum(1 for x in block if re.search(r'# Example\s+[123]\b',x))
                    out[name]={'file':path.name,'line':j+1,'examples':examples}
            else:
                i+=1
    return out

def lexical_balance(path):
    text=path.read_text(encoding='utf-8')
    stack=[]; pairs={')':'(',']':'[','}':'{'}
    state='code'; quote=None; esc=False; line=1
    for c in text:
        if c=='\n':
            line+=1
            if state=='comment': state='code'
            continue
        if state=='comment': continue
        if state=='string':
            if esc: esc=False; continue
            if c=='\\': esc=True; continue
            if c==quote: state='code'; quote=None
            continue
        if state=='backtick':
            if esc: esc=False; continue
            if c=='\\': esc=True; continue
            if c=='`': state='code'
            continue
        if c=='#': state='comment'; continue
        if c in ('"',"'"): state='string'; quote=c; continue
        if c=='`': state='backtick'; continue
        if c in '([{': stack.append((c,line))
        elif c in ')]}':
            if not stack or stack[-1][0]!=pairs[c]: return False, f"mismatch {c} at line {line}"
            stack.pop()
    if state=='string': return False, f"unterminated string at EOF"
    if state=='backtick': return False, f"unterminated backtick at EOF"
    if stack: return False, f"unclosed {stack[-1][0]} from line {stack[-1][1]}"
    return True,'balanced'

checks=[]
def check(name, ok, detail=''):
    checks.append({'check':name,'ok':bool(ok),'detail':detail})

exports=exported_blocks(); names=set(exports)
check('77 public functions detected',len(names)==77,f'{len(names)} detected')
check('three roxygen examples per public function',all(v['examples']>=3 for v in exports.values()),', '.join(k for k,v in exports.items() if v['examples']<3) or 'all >= 3')

ns_text=(pkg/'NAMESPACE').read_text(encoding='utf-8')
ns=set(re.findall(r'^export\(([^)]+)\)',ns_text,re.M))
check('NAMESPACE synchronized with roxygen exports',ns==names,f'missing={sorted(names-ns)} extra={sorted(ns-names)}')
check('NAMESPACE declares roxygen regeneration', 'roxygen2' in ns_text.lower(), ns_text.splitlines()[0] if ns_text else '')

with (pkg/'inst/metadata/block_registry.csv').open(encoding='utf-8',newline='') as fh:
    blocks=list(csv.DictReader(fh))
block_names={r['primary_function'] for r in blocks}
check('70 primary blocks registered',len(blocks)==70 and [int(r['block']) for r in blocks]==list(range(1,71)),f'{len(blocks)} rows')
check('all primary blocks implemented and exported',all(r['status']=='implemented' and r['primary_function'] in names for r in blocks),', '.join(r['primary_function'] for r in blocks if r['status']!='implemented' or r['primary_function'] not in names))

man={x.stem for x in (pkg/'man').glob('*.Rd')}
check('Rd snapshot covers all public functions',names <= man,f'missing={sorted(names-man)}')

api=next(iter(sorted((pkg/'vignettes').glob('*api-example-catalog.Rmd'))), pkg/'vignettes/v20-api-example-catalog.Rmd')
api_text=api.read_text(encoding='utf-8') if api.exists() else ''
missing_api=[x for x in sorted(names) if f'## `{x}()`' not in api_text]
check('API vignette covers all public functions',not missing_api,f'missing={missing_api}')
check('at least 20 vignettes',len(list((pkg/'vignettes').glob('*.Rmd')))>=20,str(len(list((pkg/'vignettes').glob('*.Rmd')))))

bad=[]
for path in sorted(Rdir.glob('*.R')):
    ok,detail=lexical_balance(path)
    if not ok: bad.append(f'{path.name}: {detail}')
check('R-source lexical delimiter balance',not bad,'; '.join(bad) or 'all R files balanced')

# The balance check above counts delimiters only. It cannot detect invalid R syntax
# such as list(1 = ...), which is why an unparseable R/ tree once passed this gate.
# When an R runtime is present, parse every source file for real.
_rscript = shutil.which('Rscript')
if _rscript:
    _code = ('bad <- character(0); '
             'for (f in list.files(commandArgs(TRUE)[1], pattern = "[.]R$", full.names = TRUE)) '
             'bad <- c(bad, tryCatch({parse(f); character(0)}, '
             'error = function(e) paste0(basename(f), ": ", conditionMessage(e)))); '
             'if (length(bad)) cat(paste(bad, collapse = "; ")) else cat("all R files parse")')
    _proc = subprocess.run([_rscript, '--vanilla', '-e', _code, str(Rdir)],
                           capture_output=True, text=True)
    _out = (_proc.stdout or '').strip() or (_proc.stderr or '').strip()
    check('R sources parse under the R runtime', _proc.returncode == 0 and _out == 'all R files parse', _out)
else:
    check('R sources parse under the R runtime', False, 'not performed: Rscript unavailable')

place=[]
for path in sorted(Rdir.glob('*.R')):
    for n,line in enumerate(path.read_text(encoding='utf-8').splitlines(),1):
        if re.search(r'\b(TODO|FIXME|NOT IMPLEMENTED|PLACEHOLDER)\b',line,re.I): place.append(f'{path.name}:{n}')
check('no provisional markers in R source',not place,', '.join(place) or 'none')

# Dataset integrity
meta={}
with (pkg/'inst/metadata/datasets.csv').open(encoding='utf-8',newline='') as fh:
    for r in csv.DictReader(fh): meta[r['name']]=r
mis=[]
for csvp in sorted((pkg/'inst/extdata').glob('*.csv')):
    key=csvp.stem
    sha=hashlib.sha256(csvp.read_bytes()).hexdigest()
    if key not in meta or meta[key].get('sha256')!=sha: mis.append(key)
check('frozen dataset SHA256 metadata',not mis,', '.join(mis) or 'all datasets match metadata')

required=['DESCRIPTION','NAMESPACE','README.md','NEWS.md','ARCHITECTURE.md','LOCAL_VALIDATION_WINDOWS.md','tests/testthat.R','tools/regenerate_docs.R','tools/validate_local.R','inst/metadata/simulation_scenarios.csv']
missing=[x for x in required if not (pkg/x).exists()]
check('release-support files present',not missing,', '.join(missing) or 'all present')

desc=(pkg/'DESCRIPTION').read_text(encoding='utf-8')
check('author/maintainer metadata', all(x in desc for x in ['Walter','0000-0003-1085-0191','walterufpb@yahoo.com.br','Magali','0009-0009-5419-959X']),'required author metadata found')
check('roxygen2 target declared','roxygen2 (>= 8.1.0)' in desc and 'RoxygenNote: 8.1.0' in desc,'roxygen2 8.1.0')

r=shutil.which('R'); rs=shutil.which('Rscript')
runtime_available=bool(r or rs)
check('R runtime available in build environment',runtime_available,f'R={r}; Rscript={rs}')

result={
    'package':'mixedFlowR','version':'0.1.0.9000','audit_type':'static_source_audit',
    'runtime_R_validation_performed':False,
    'runtime_R_available':runtime_available,
    'public_functions':len(names),'primary_blocks':len(blocks),
    'vignettes':len(list((pkg/'vignettes').glob('*.Rmd'))),
    'tests':len(list((pkg/'tests/testthat').glob('test-*.R'))),
    'checks':checks,
    'static_pass':all(c['ok'] for c in checks
                      if c['check']!='R runtime available in build environment'
                      and not str(c['detail']).startswith('not performed'))
}
(pkg/'STATIC_VALIDATION.json').write_text(json.dumps(result,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
lines=['# mixedFlowR Static Validation Report','','This report checks source integrity only. It is not a substitute for `R CMD check` or numerical/runtime validation.','','| Check | Status | Detail |','|---|---|---|']
for c in checks:
    detail=str(c['detail']).replace('|','\\|')
    lines.append(f"| {c['check']} | {'PASS' if c['ok'] else 'PENDING/FAIL'} | {detail} |")
lines += ['',f"Static source gate: **{'PASS' if result['static_pass'] else 'FAIL'}**.",'']
if runtime_available:
    lines += ['Runtime R validation: **PENDING**. An R runtime is available here, so run `tools/validate_local.R` and record its log before release.']
else:
    lines += ['Runtime R validation: **PENDING**. The construction environment does not provide R/Rscript; run `tools/validate_local.R` on the target R installation before release.']
(pkg/'STATIC_VALIDATION.md').write_text('\n'.join(lines)+'\n',encoding='utf-8')

state={'package':'mixedFlowR','version':'0.1.0.9000','blocks_implemented':70,'public_functions':77,'static_source_gate':'pass' if result['static_pass'] else 'fail','runtime_R_validation':'pending','reason':'awaiting recorded tools/validate_local.R run' if runtime_available else 'R/Rscript unavailable in construction environment','local_validation':'LOCAL_VALIDATION_WINDOWS.md'}
(pkg/'inst/metadata/build_state.json').write_text(json.dumps(state,indent=2)+'\n',encoding='utf-8')
print(json.dumps(result,indent=2,ensure_ascii=False))
sys.exit(0 if result['static_pass'] else 2)
