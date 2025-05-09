#' @title Create Random Names
#' @description Generates a list of random names.
#'
#' @param num_names The number of names to generate.
#'
#' @export
#' @returns A character vector of random names.
#'
#' @examples
#' create_random_names(num_names = 10)
create_random_names <- function(num_names = 100) {
  generate_name(num_names)
}

#' @title Grab First Names
#' @description Returns a list of common first names.
#' @returns A character vector of first names.
grab_first_names <- function() {
  c(
    "Olivia",
    "Liam",
    "Emma",
    "Noah",
    "Amelia",
    "Oliver",
    "Ava",
    "Elijah",
    "Sophia",
    "Mateo",
    "Isabella",
    "Lucas",
    "Mia",
    "Levi",
    "Charlotte",
    "Asher",
    "Luna",
    "James",
    "Gianna",
    "Leo",
    "John",
    "Mary",
    "Robert",
    "Patricia",
    "Michael",
    "Jennifer",
    "William",
    "Linda",
    "David",
    "Elizabeth",
    "Richard",
    "Barbara",
    "Joseph",
    "Susan",
    "Thomas",
    "Jessica",
    "Charles",
    "Sarah",
    "Christopher",
    "Karen"
  )
}

#' @title Grab Last Names
#' @description Returns a list of common last names.
#' @returns A character vector of last names.
grab_last_names <- function() {
  last_names <- c(
    "Smith",
    "Johnson",
    "Williams",
    "Brown",
    "Jones",
    "Garcia",
    "Miller",
    "Davis",
    "Rodriguez",
    "Martinez",
    "Hernandez",
    "Lopez",
    "Gonzalez",
    "Wilson",
    "Anderson",
    "Thomas",
    "Taylor",
    "Moore",
    "Martin",
    "Jackson",
    "Lee",
    "Perez",
    "Thompson",
    "White",
    "Harris",
    "Sanchez",
    "Clark",
    "Ramirez",
    "Lewis",
    "Robinson",
    "Walker",
    "Young",
    "Allen",
    "King",
    "Wright",
    "Scott",
    "Green",
    "Baker",
    "Adams",
    "Nelson"
  )
}

#' @title Generate Name Type
#' @description Randomly assigns a name type (first, last, or both).
#' @param num_names The number of names to generate.
#' @returns A tibble with name types.
generate_name_type <- function(num_names) {
  # Decide name type: 1 = First only, 2 = Last only, 3 = First + Last
  sample(1:3, num_names, replace = TRUE) |>
    tibble::as_tibble_col("name_type")
}

#' @title Generate Random Transform
#' @description Randomly assigns a case transformation to names.
#' @param num_names The number of names to generate.
#' @returns A tibble with name transformations.
generate_random_transform <- function(num_names) {
  # Decide case transformation: 1 = As is, 2 = Lowercase, 3 = Uppercase
  sample(1:3, num_names, replace = TRUE) |>
    tibble::as_tibble_col("name_transform")
}

#' @title Generate Name
#' @description Generates random names with transformations.
#' @param num_names The number of names to generate.
#' @returns A character vector of generated names.
generate_name <- function(num_names) {
  name <- NULL

  random_names <-
    dplyr::bind_cols(
      generate_name_type(num_names),
      generate_random_transform(num_names)
    )

  first_names <- grab_first_names()
  last_names <- grab_last_names()

  names <-
    purrr::map_chr(
      random_names$name_type,
      \(name_type) {
        if (name_type == 1) {
          name_string <- sample(first_names, 1)
        } else if (name_type == 2) {
          name_string <- sample(last_names, 1)
        } else {
          first <- sample(first_names, 1)
          last <- sample(last_names, 1)
          name_string <- paste(first, last)
        }
      }
    ) |>
    tibble::as_tibble_col("name")

  dplyr::bind_cols(
    names,
    random_names
  ) |>
    dplyr::mutate(
      name = dplyr::case_when(
        name_transform == 1 ~ name,
        name_transform == 2 ~ stringr::str_to_lower(name),
        name_transform == 3 ~ stringr::str_to_upper(name),
      )
    ) |>
    dplyr::pull(name)
}
