context("Test: getGroups()")

test_that("The getGroups() interface works properly... test: archaea",{

    skip_on_cran()
    skip_on_travis()

    expect_equal(
        getGroups("archaea", db = "refseq"),
        c("Archaeoglobi", "Candidatus Bathyarchaeota",
          "Candidatus Culexarchaeota", "Candidatus Korarchaeota",
          "Candidatus Lokiarchaeota", "Candidatus Micrarchaeota",
          "Candidatus Nanohaloarchaeota", "Methanomada group",
          "Methanonatronarchaeia", "Nanoarchaeota",
          "Nitrososphaerota", "Stenosarchaea group",
          "Thermococci", "Thermoplasmata", "Thermoproteota")
    )
})

test_that("The getGroups() interface works properly... test: bacteria",{

    skip_on_cran()
    skip_on_travis()
    # Too many groups now
    # just test that it outputs correct format and does not fail
    expect_type(
        getGroups("bacteria", db = "refseq"),
        "character"
    )
})

test_that("The getGroups() interface works properly... test: fungi",{

    skip_on_cran()
    skip_on_travis()

    expect_equal(
        getGroups("fungi", db = "refseq"),
        c(
            "Ascomycetes", "Basidiomycetes", "Other Fungi"
        )
    )
})


test_that("The getGroups() interface works properly... test: invertebrate",{

    skip_on_cran()
    skip_on_travis()

    expect_equal(
        getGroups("invertebrate", db = "refseq"),
        c(
            "Flatworms", "Insects", "Other Animals", "Roundworms"
        )
    )
})


test_that("The getGroups() interface works properly... test: plant",{

    skip_on_cran()
    skip_on_travis()

    expect_equal(
        getGroups("plant", db = "refseq"),
        c(
            "Green Algae", "Land Plants"
        )
    )
})


test_that("The getGroups() interface works properly... test: protozoa",{

    skip_on_cran()
    skip_on_travis()

    expect_equal(
        getGroups("protozoa", db = "refseq"),
        c(
            "Apicomplexans", "Kinetoplasts", "Other Protists"
        )
    )
})

test_that("The getGroups() interface works properly... test:
          vertebrate_mammalian",{

              skip_on_cran()
              skip_on_travis()

    expect_equal(
        getGroups("vertebrate_mammalian", db = "refseq"),
        c(
            "Mammals"
        )
    )
})

test_that("The getGroups() interface works properly...
          test: vertebrate_other",{

              skip_on_cran()
              skip_on_travis()

    expect_equal(
        getGroups("vertebrate_other", db = "refseq"),
        c(
            "Amphibians", "Birds", "Fishes", "Reptiles"
        )
    )
})










