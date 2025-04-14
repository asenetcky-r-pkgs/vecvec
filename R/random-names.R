grab_random_names <- function(num_names = 100) {
  generate_name(num_names)
}

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

generate_name_type <- function(num_names) {
  # Decide name type: 1 = First only, 2 = Last only, 3 = First + Last
  sample(1:3, num_names, replace = TRUE) |>
    tibble::as_tibble_col("name_type")
}

generate_random_transform <- function(num_names) {
  # Decide case transformation: 1 = As is, 2 = Lowercase, 3 = Uppercase
  sample(1:3, num_names, replace = TRUE) |>
    tibble::as_tibble_col("name_transform")
}

generate_name <- function(num_names) {
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
