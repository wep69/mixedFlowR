test_that("all 70 primary blocks are registered", {
  z <- mixed_blocks()
  expect_equal(nrow(z), 70L)
  expect_equal(z$block, 1:70)
  expect_false(anyDuplicated(z$function_name) > 0L)
  expect_true(all(z$status == "implemented"))
})

test_that("the internal registry is positional and complete", {
  # WHY: mixed_blocks() reads the registry by position (seq_along + [[i]]), so the
  # block number a user sees comes from the ORDER of the list, not from its tags.
  # The tags therefore have to stay "1".."70" in order, otherwise a printed block
  # number would point to a different scientific step than the source suggests.
  reg <- get(".mixedflow_blocks", envir = asNamespace("mixedFlowR"))
  expect_length(reg, 70L)
  expect_identical(names(reg), as.character(seq_len(70)))
  expect_true(all(vapply(reg, function(x)
    identical(sort(names(x)), c("function_name", "module", "status")), logical(1))))
})

test_that("the block registry names functions that really exist and are exported", {
  # WHY: the registry is hand-maintained metadata. If it drifts from the code, the
  # package advertises a scientific workflow it cannot deliver and mixed_blocks()
  # becomes a list of promises instead of a map of the public API.
  z <- mixed_blocks()
  expect_true(all(z$function_name %in% getNamespaceExports("mixedFlowR")))
  expect_true(all(vapply(z$function_name,
    function(f) is.function(get0(f, envir = asNamespace("mixedFlowR"))), logical(1))))
})

test_that("the shipped block metadata matches the internal registry", {
  # WHY: inst/metadata/block_registry.csv ships to users and feeds the static audit.
  # Two registries that disagree would let one of them be silently wrong.
  csv_path <- system.file("metadata", "block_registry.csv", package = "mixedFlowR")
  expect_true(nzchar(csv_path))
  b <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  z <- mixed_blocks()
  expect_equal(as.integer(b$block), z$block)
  expect_equal(b$primary_function, z$function_name)
  expect_equal(b$module, z$module)
  expect_equal(b$status, z$status)
})

test_that("frozen teaching datasets have the declared structures", {
  expect_named(mixed_data("splitplot"), c("block","variety","nitrogen","yield"))
  expect_named(mixed_data("longitudinal"), c("subject","treatment","time","height"))
  b <- mixed_data("beta")
  expect_true(all(b$severity > 0 & b$severity < 1))
  q <- mixed_data("binomial")
  expect_equal(q$diseased + q$healthy, q$total)
})
