test_that("site_table_add_row expands the sites table and handles inputs", {
  ts_file <- system.file("examples/test.trees", package = "RcppTskit")
  tc_xptr <- rtsk_table_collection_load(ts_file)

  n_before <- as.integer(rtsk_table_collection_get_num_sites(tc_xptr))
  m_before <- as.integer(rtsk_table_collection_metadata_length(tc_xptr)[[
    "sites"
  ]])

  new_id <- rtsk_site_table_add_row(
    tc = tc_xptr,
    position = 12.5,
    ancestral_state = charToRaw("A"),
    metadata = charToRaw("m")
  )
  expect_equal(new_id, n_before)
  expect_equal(
    as.integer(rtsk_table_collection_get_num_sites(tc_xptr)),
    n_before + 1L
  )
  expect_equal(
    as.integer(rtsk_table_collection_metadata_length(tc_xptr)[["sites"]]),
    m_before + 1L
  )

  # Defaults should be accepted (generated R wrapper maps missing args to NULL)
  id2 <- rtsk_site_table_add_row(tc_xptr)
  expect_equal(id2, n_before + 1L)
  expect_equal(
    as.integer(rtsk_table_collection_get_num_sites(tc_xptr)),
    n_before + 2L
  )
})


test_that("mutation_table_add_row expands the mutations table and handles inputs", {
  ts_file <- system.file("examples/test.trees", package = "RcppTskit")
  tc_xptr <- rtsk_table_collection_load(ts_file)

  n_before_mut <- as.integer(rtsk_table_collection_get_num_mutations(tc_xptr))

  # Ensure we have a site to attach the mutation to.
  site_id <- rtsk_site_table_add_row(
    tc_xptr,
    position = 1.0,
    ancestral_state = charToRaw("G")
  )
  expect_true(as.integer(rtsk_table_collection_get_num_sites(tc_xptr)) >= 1L)

  new_mut_id <- rtsk_mutation_table_add_row(
    tc = tc_xptr,
    site = as.integer(site_id),
    node = 0L,
    parent = -1L,
    time = 0.0,
    derived_state = charToRaw("T"),
    metadata = raw()
  )
  expect_equal(new_mut_id, n_before_mut)
  expect_equal(
    as.integer(rtsk_table_collection_get_num_mutations(tc_xptr)),
    n_before_mut + 1L
  )

  # Defaults should be accepted for optional ragged columns
  id2 <- rtsk_mutation_table_add_row(
    tc_xptr,
    site = as.integer(site_id),
    node = 0L
  )
  expect_equal(id2, n_before_mut + 1L)
  expect_equal(
    as.integer(rtsk_table_collection_get_num_mutations(tc_xptr)),
    n_before_mut + 2L
  )
})
