# Relatório ao autor do pacote `mixedFlowR`

## Assunto e escopo

Este relatório acompanha o tutorial `mixedFlowR-tutorial-completo.qmd` e organiza, para quem mantém o pacote, os achados e as limitações observados na auditoria. Nenhum arquivo do pacote foi alterado.

A cobertura do ciclo final foi registrada em `outputs/api_audit/api_coverage_render.csv`: **77 funções, 72 `executed`, 5 `blocked` e 0 com status=error**. As cinco funções bloqueadas são `mixed_bayes`, `mixed_loo`, `mixed_posterior`, `mixed_pp_check` e `mixed_prior_predictive`.

> **Nota sobre status e campo `error`:** as cinco linhas com status `blocked` possuem mensagem preenchida no campo `error`, porque o runner registra nesse campo a causa do bloqueio. Portanto, a contagem de registros com status=error é diferente da contagem de linhas com campo `error` preenchido.

Os renders finais produziram HTML e PDF com sucesso. O log HTML registra `exit_code=0`, saída autônoma e `embed-resources: true`; o log PDF registra `Output created` e `exit_code=0`. O gate visual também foi concluído e está documentado em `outputs/reports/pdf_visual_qa.md`, com veredito **APROVADO COM RESSALVA DE COBERTURA**.

## Evidências do ciclo final e evidência histórica

### Evidências do ciclo final

- `outputs/api_audit/api_coverage_render.csv`: inventário e estados das 77 funções no render atual;
- `outputs/render/tutorial_html.log`: HTML final criado, com recursos incorporados, e `exit_code=0`;
- `outputs/render/tutorial_pdf.log`: PDF final criado por LuaLaTeX e `exit_code=0`;
- `outputs/reports/pdf_visual_qa.md`: gate visual concluído, numeração duplicada eliminada e ausência observada de texto cortado, figuras sobrepostas, tabelas ilegíveis e página vazia.

A ressalva do gate visual é de cobertura: a ferramenta disponível não realizou rasterização nativa de 100% das páginas do PDF. A inspeção foi realizada no HTML final equivalente e ancorada pelo log definitivo do PDF. Isso limita a cobertura do teste, mas o gate visual possui relatório e veredito registrados.

> **Nota técnica:** o CSV possui 77 registros lógicos e 77 valores únicos em `function_name`, embora tenha 86 linhas físicas em razão de oito quebras de linha dentro de campos *quoted* de `warnings`/`messages`. Linhas físicas não devem ser contadas como registros.

### Evidência histórica

`logs/etapa8_render4.log` registra 20 de 20 vignettes renderizadas com status `OK`. Esse arquivo é evidência histórica do ciclo das vignettes e não deve ser usado como evidência do ciclo final atual do tutorial, da cobertura da API, do HTML, do PDF ou do QA visual.

## Como ler a severidade

| Severidade | Critério |
|---|---|
| Alta | O usuário recebe número ou objeto de aparência normal, mas o resultado não corresponde ao pedido, sem sinalização. |
| Média | O resultado exige uma reconciliação manual, uma dependência opcional não registrada ou uma limitação que pode afetar a interpretação. |
| Baixa | Mensagem, interface, documentação ou organização dificultam o uso, mas não alteram o resultado científico. |
| Sem defeito associado | Gate, inventário ou limitação de ambiente registrado para controle da validação; não é classificação do pacote como incorreto. |

## Resumo executivo

| ID | Item | Severidade | Esforço | Estado |
|---|---|---:|---:|---|
| R01 | Estado da bateria de simulação conflita entre metadado e log de release | Média | Pequeno | Confirmado documentalmente |
| R02 | Gate de validação runtime, render e inspeção visual | Sem defeito associado | Pequeno para registro | Concluído com ressalva de cobertura; não é defeito do pacote |
| R03 | Workspace contém biblioteca compilada e fontes pré-reflow, mas não uma árvore R atual | Baixa | Médio | Confirmado documentalmente |
| R04 | Falha antiga de parse em cópia de fonte não está reconciliada com o snapshot atual | Média como risco documental | Médio | Evidência histórica; não classificada como defeito atual |
| R05 | Cobertura da API no render | Sem defeito associado | Médio para ampliar a execução | Inventário completo: 77/72/5/0; cinco funções bloqueadas |
| R06 | Incompatibilidade entre versões de `TMB` e `glmmTMB` | Média | Médio | Aviso confirmado; impacto requer validação |
| R07 | Aviso de sobrescrita de método S3 por `clubSandwich` | Baixa | Pequeno | Aviso observado; ordem de carregamento ainda precisa ser caracterizada |
| R08 | Avisos de convergência nos cenários espacial e Toeplitz | Média como limitação de cenário | Médio | Achado de validação, não defeito confirmado do pacote |
| R09 | Caminho Bayes bloqueado pelo ambiente Stan/C++17 | Média como limitação de reprodução | Médio a alto | Limitação do ambiente; sem evidência de defeito do pacote |

## R01. Estado da bateria de simulação

### Sintoma

`metadata/simulation_scenarios.csv` marca SIM01 a SIM12 como `defined_not_run`, enquanto `logs/ETAPA_RELEASE_GATE.md` declara quatro cenários executados.

### Evidência

A diferença pode ser explicada por versões diferentes da bateria, mas os arquivos não apresentam uma relação explícita entre o log e os metadados.

### Correção sugerida

Adicionar ao manifesto os campos de rastreabilidade:

```yaml
execution_id: <identificador>
source_commit: <commit ou hash do snapshot>
executed_at: <data UTC>
status: executed
log: <caminho>
```

### Teste de regressão sugerido

Um teste de documentação deve falhar quando `status` indicar execução e não houver `execution_id`, data, origem do código e caminho para o log correspondente.

## R02. Gate de validação runtime, render e inspeção visual

### Sintoma

Este item **não descreve defeito do pacote**. O gate foi concluído e registra:

- 77 funções no CSV final, sendo 72 `executed`, 5 `blocked` e 0 com status=error;
- HTML final criado com `exit_code=0` e recursos incorporados;
- PDF final criado com `exit_code=0`;
- gate visual concluído, **APROVADO COM RESSALVA DE COBERTURA**;
- numeração duplicada eliminada;
- nenhum texto cortado, figura sobreposta, tabela ilegível ou página vazia observado na inspeção realizada.

A ressalva é de cobertura: não houve rasterização nativa de 100% das páginas do PDF.

### Correção sugerida

Atualizar o manifesto de build para apontar para `outputs/api_audit/api_coverage_render.csv`, para os logs finais de HTML e PDF e para `outputs/reports/pdf_visual_qa.md`, registrando o veredito e a ressalva de cobertura. Não classificar R02 como defeito do pacote.

Se a política de publicação exigir inspeção nativa de todas as páginas, essa política deve produzir um gate suplementar posterior. Isso é uma ampliação de cobertura, não uma etapa ausente do relatório visual atual.

### Teste de regressão sugerido

O pipeline de release deve:

1. conferir a igualdade entre o número de exportações e o número de registros lógicos de cobertura (parse CSV);
2. preservar `blocked` sem convertê-lo em `executed` ou sucesso;
3. falhar se o HTML ou o PDF não registrarem saída criada e `exit_code=0`;
4. exigir um relatório de QA visual com veredito, ressalvas e cobertura;
5. aplicar uma regra adicional de cobertura total somente se essa exigência estiver declarada na política de release.

## R03. Fonte operacional não está na raiz

### Sintoma

A biblioteca instalada possui o pacote compilado e os metadados, mas a árvore de desenvolvimento `R/` não está disponível na raiz do workspace. As cópias em `logs/R.pre-reflow` são evidência histórica, não fonte operacional atual.

### Correção sugerida

Manter um diretório de trabalho `source/` ou usar o snapshot de código-fonte como entrada de build. Registrar o hash do snapshot antes de qualquer alteração.

### Teste de regressão sugerido

A rotina de build deve comparar o hash do snapshot com `metadata/source_manifest.csv` e falhar se o conteúdo usado não estiver no manifesto.

## R04. Falha de parse histórica

### Sintoma

`logs/static_audit_regressao.json` registra `R/block-registry.R:3:5`, `= inesperado`, em uma cópia de fonte. O arquivo não demonstra, por si só, que a fonte usada no snapshot atual contém a mesma falha.

### Investigação necessária

1. identificar o hash da cópia usada nesse log;
2. comparar essa cópia com o código-fonte do snapshot atual;
3. executar o parse em um ambiente limpo sobre a fonte usada pelo build;
4. somente então classificar o problema como defeito atual ou evidência histórica.

### Correção sugerida

Não aplicar correção no pacote instalado com base apenas em um log antigo. Primeiro reproduzir a falha na mesma cópia e confirmar que a origem usada pelo build contém o trecho afetado.

### Teste de regressão sugerido

Adicionar um teste estático de parse para cada arquivo R do manifesto e armazenar o resultado junto do build. O teste deve usar o mesmo código-fonte que será compilado.

## R05. Cobertura da API no render

### Sintoma

O inventário do render cobre todas as 77 funções, mas cinco não foram executadas. O estado final é **77 funções, 72 `executed`, 5 `blocked` e 0 com status=error**. O inventário está completo; a execução integral ainda não está demonstrada neste ambiente.

As funções bloqueadas são:

- `mixed_bayes`;
- `mixed_loo`;
- `mixed_posterior`;
- `mixed_pp_check`;
- `mixed_prior_predictive`.

### Evidência

`outputs/api_audit/api_coverage_render.csv` preserva a distinção entre `executed`, `blocked` e `error`. O CSV tem 77 registros lógicos e 86 linhas físicas, mas 77 nomes únicos em `function_name`; o inventário é definido pelos 77 registros lógicos, não pelas linhas físicas. `mixed_bayes` foi bloqueada pela mensagem `Stan heavy path not enabled`; as outras quatro dependem de um `brms_fit` que não estava disponível.

### Leitura para o autor

Esse resultado é uma limitação controlada da cobertura de execução, não evidência de defeito do pacote. Ele mostra exatamente quais funções permanecem fora da execução verificada e evita que uma limitação de ambiente seja apresentada como sucesso integral.

### Correção sugerida

Manter o runner derivado de `getNamespaceExports()` e fazer o relatório de release consumir o CSV gerado no mesmo render. Tratar `blocked` como estado reproduzível e informativo. Para elevar a cobertura de execução, habilitar o ambiente descrito em R09 e repetir essas cinco funções sem alterar o pacote apenas para modificar o relatório.

### Teste de regressão sugerido

A cobertura final deve ser derivada de `getNamespaceExports()` no mesmo render. O teste deve exigir que:

- o número de funções inventariadas coincida com o número de exportações;
- o parse do CSV retorne 77 registros, 77 nomes únicos e os estados somem 77;
- funções não executadas apareçam como `blocked` ou `not_run`, nunca como `passed`;
- a lista de funções bloqueadas seja preservada e registrada explicitamente;
- uma função deixe de constar como `blocked` somente após execução efetiva com registro do resultado.

## R06. Incompatibilidade entre versões de `TMB` e `glmmTMB`

### Sintoma

Durante a sondagem segura de `mixed_capabilities`, o runtime emitiu um aviso informando que `glmmTMB` foi compilado com a versão 1.9.23 de `TMB`, enquanto a versão atual de `TMB` é 1.9.25. A mensagem recomenda reinstalar `glmmTMB` a partir do código-fonte ou restaurar a versão original de `TMB`.

### Evidência

A evidência está em `outputs/api_audit/safe_runtime_probes.csv`, na linha da sondagem `mixed_capabilities`, cujo status é `executed` e cujo campo de avisos contém as duas versões e a recomendação. A divergência pode afetar o backend, mas o impacto sobre modelos representativos ainda precisa ser medido.

### Correção sugerida

Reinstalar `glmmTMB` em ambiente controlado usando a versão atual de `TMB`, ou fixar um conjunto de versões compatíveis e registrá-lo no manifesto do ambiente. Testar a escolha antes de alterar a instalação compartilhada.

### Teste de regressão sugerido

Em um ambiente reproduzível, registrar `packageVersion("TMB")` e os metadados de `glmmTMB` antes das sondagens; executar um cenário representativo dependente do backend `glmmTMB`; falhar se o aviso de incompatibilidade reaparecer; e comparar o resultado antes e depois da reinstalação ou do pinning.

## R07. Aviso de sobrescrita de método S3 por `clubSandwich`

### Sintoma

A sondagem `mixed_capabilities` registrou o aviso `Registered S3 method overwritten by 'clubSandwich'`, indicando a sobrescrita de `bread.mlm` associado a `sandwich`.

### Evidência

O aviso está em `outputs/api_audit/safe_runtime_probes.csv`, na linha da sondagem `mixed_capabilities`, no ciclo final de runtime. A ordem de carregamento de `sandwich` e `clubSandwich` ainda não foi caracterizada nesta auditoria; por isso, não há base para atribuir o aviso ao pacote.

### Correção sugerida

Antes de alterar o pacote, verificar a ordem de carregamento, comparar `methods("bread.mlm")` e registrar o ambiente de carregamento. Se a ordem for intencional, documentar a expectativa e considerar uma carga controlada. Se for acidental, ajustar a sequência de namespaces ou a inicialização da sessão de auditoria. Não afirmar defeito do pacote antes dessa verificação.

### Teste de regressão sugerido

Executar a auditoria em sessão limpa em duas ordens controladas de carregamento, capturar `methods("bread.mlm")` antes e depois e exigir que o teste documente a origem da sobrescrita. O teste deve falhar quando a ordem observada produzir uma sobrescrita inesperada, não apenas pela presença isolada do aviso.

## R08. Avisos de convergência nos cenários espacial e Toeplitz

### Sintoma

`mixed_spatial_covariance()` e `mixed_toeplitz()` foram executadas, mas o CSV registra `NA/NaN function evaluation`, `Model convergence problem; non-positive-definite Hessian matrix` e `false convergence (8)` para ambas. O retorno é `mixedflow_fit` e o status é `executed`.

### Leitura para o autor

Este é um achado de validação de cenário, não uma classificação automática de defeito. Os avisos indicam que a solução numérica desses cenários deve ser lida com cautela, mas não provam que a API, o retorno ou o pacote estejam incorretos. Escala, desenho dos dados, identificação e estabilidade do modelo continuam hipóteses abertas.

### Correção sugerida

Antes de alterar o pacote, repetir cada cenário com dados alternativos e registrar `diagnose()`, a matriz de Hessian, os avisos e a comparação com o modelo de referência. Se a não convergência persistir em cenários equivalentes e puder ser reproduzida com controles adequados, abrir uma investigação de implementação.

### Teste de regressão sugerido

Adicionar um teste de cenário para os dois ajustes que exija registro dos avisos, diagnóstico numérico disponível e comparação explícita com o resultado de referência. O teste não deve falhar apenas pela presença do aviso; deve falhar quando o resultado de referência não puder ser interpretado sem o diagnóstico esperado.

## R09. Caminho Bayes bloqueado pelo toolchain Stan/C++17

### Sintoma

Cinco funções permanecem `blocked` no ciclo final:

- `mixed_bayes`, com a mensagem `Stan heavy path not enabled`;
- `mixed_loo`, `mixed_posterior`, `mixed_pp_check` e `mixed_prior_predictive`, com registro de ausência de `brms_fit`.

### Distinção entre ambiente e pacote

O estado observado é de caminho Bayes não habilitado no toolchain Stan/C++17 disponível para a execução. É uma limitação de ambiente e de reprodução. Não há evidência suficiente para chamar `mixedFlowR` de defeituoso, nem para afirmar que a lógica Bayes do pacote está incorreta.

### Correção sugerida

Fornecer um procedimento reproduzível para habilitar o caminho Stan/C++17, fixar as versões do compilador, Stan, R e dependências e repetir as cinco funções. Registrar o comando, o `sessionInfo()`, o log e o resultado individual de cada função.

### Teste de regressão sugerido

O pipeline deve testar primeiro o ambiente e falhar imediatamente se o toolchain necessário não estiver disponível, com mensagem `Stan heavy path not enabled`. Em ambiente habilitado, deve exigir que `mixed_bayes` e as quatro funções dependentes deixem de aparecer como `blocked`. Somente essa repetição separa limitação de ambiente de defeito do pacote.

## O que já está evidenciado

- O CSV final separa corretamente `executed`, `blocked` e `error`, sem apresentar as cinco limitações como sucesso.
- O HTML final foi criado com saída autônoma, recursos incorporados e `exit_code=0`.
- O PDF final foi criado e compilou com `exit_code=0`.
- O gate visual foi concluído e emitido relatório.
- A numeração duplicada observada anteriormente foi eliminada.
- Na inspeção realizada, não foram observados texto cortado, figuras sobrepostas, tabelas ilegíveis ou página vazia.

Essas observações comprovam o estado dos gates registrados; não ampliam a cobertura além do que cada evidência documenta.

## Sugestão de ordem de trabalho

A inspeção visual já foi concluída e não deve voltar à lista. A ordem abaixo trata apenas de itens ainda abertos:

1. atualizar o build state e o manifesto para apontar para o CSV de cobertura, os dois logs de render finais e o relatório de QA visual já concluído;
2. reconciliar os metadados da bateria de simulação com os logs de execução;
3. reproduzir a falha de parse histórica no mesmo código-fonte usado pelo build e classificá-la como atual ou histórica;
4. verificar a ordem de carregamento de `sandwich` e `clubSandwich` antes de classificar R07;
5. reproduzir e reconciliar a divergência `TMB`/`glmmTMB` em ambiente controlado, com pinning explícito;
6. repetir os cenários espacial e Toeplitz com diagnóstico numérico antes de propor alteração no pacote;
7. habilitar o caminho Bayes e repetir as cinco funções bloqueadas em ambiente reproduzível;
8. adicionar teste de integridade entre snapshot, manifesto e fontes e, depois, revisar documentação de API e mensagens de erro.

Se a política de release exigir cobertura visual nativa total, a rasterização página por página deve ser acrescentada como um gate suplementar e explicitamente separado do QA visual já concluído.

## Ambiente da verificação

- Sistema operacional do projeto: Windows 11 x64; plataforma R `x86_64-w64-mingw32/x64`.
- R registrado: 4.6.0 (2026-04-24 ucrt).
- Pacote: `mixedFlowR` `0.1.0.9000`.
- Biblioteca: `lib-isolada/mixedFlowR`.
- Inventário: 77 funções em `outputs/api_audit/api_formals.csv`.
- Cobertura final: 77 funções em `outputs/api_audit/api_coverage_render.csv`; 72 `executed`, 5 `blocked` e 0 com status=error.
- Funções bloqueadas: `mixed_bayes`, `mixed_loo`, `mixed_posterior`, `mixed_pp_check` e `mixed_prior_predictive`.
- Sessão: `outputs/api_audit/sessionInfo_render.txt` registra `mixedFlowR_0.1.0.9000` carregado.
- Log da auditoria: `outputs/render/api_audit.log`, `exit_code=0`.
- Log HTML final: `outputs/render/tutorial_html.log`; `standalone: true`, `embed-resources: true`, `Output created` e `exit_code=0`.
- Log PDF final: `outputs/render/tutorial_pdf.log`; LuaLaTeX, `Output created: mixedFlowR-tutorial-completo.pdf` e `exit_code=0`.
- Relatório do gate visual: `outputs/reports/pdf_visual_qa.md`; **APROVADO COM RESSALVA DE COBERTURA**. A numeração duplicada foi eliminada e não foram observados texto cortado, sobreposição ou página vazia. Não houve rasterização nativa de 100% das páginas.
- Evidência histórica das vignettes: `logs/etapa8_render4.log` registra 20 de 20 renderizações com `OK`, mas não é evidência do ciclo final atual.
- Código-fonte textual atual na raiz: não disponível.
