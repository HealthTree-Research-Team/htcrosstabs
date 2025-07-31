#' Sports by Age
#'
#' A dummy dataset of preferred sports by age group (child, teen, adult, and
#' senior)
#'
#' @format A data frame with 400 rows of preferred sports and age groups
#' \describe{
#'   \item{sport}{Preferred sport (factor)}
#'   \item{age}{Age group: child, teen, adult, or senior (factor)}
#' }
#' @source Dummy data, invented by the developer
"sports_by_age"

#' Petal Length by Species
#'
#' The petal length and species columns from the famous "iris" dataset built into R.
#'
#' @format A data frame with 150 rows of iris petal lengths and species
#' \describe{
#'   \item{petal length}{Length of flower petals in cm (numeric)}
#'   \item{species}{Species of iris: virginica, versicolor, and setosa (factor)}
#' }
#' @source The famous "iris" dataset built into R
"length_by_species"

#' Opinions on Black Licorice by US Region
#'
#' A dummy dataset of Likert-like opinions on black licorice by US region (west,
#' northeast, north central, and south). The region breakdown is inspired by the
#' `state.region` dataset.
#'
#' @format A data frame with 400 rows of black licorice opinions and US region.
#' \describe{
#'   \item{opinion}{Opinion on black licorice (factor)}
#'   \item{region}{Region of the US: west, northeast, north central, and south (factor)}
#' }
#' @source Dummy data, invented by the developer
"licorice_by_region"

#' Job Satisfaction by Company
#'
#' A dummy dataset of Likert-like satisfaction responses about working at
#' different tech companies (Apple, Amazon, Google, and Microsoft).
#'
#' @format A data frame with 400 rows of satisfaction responses about working at various companies.
#' \describe{
#'   \item{satisfaction}{Job satisfaction (factor)}
#'   \item{company}{Tech company: Apple, Amazon, Google, and Microsoft (factor)}
#' }
#' @source Dummy data, invented by the developer
"satisfaction_by_company"

#' Allergies by High School
#'
#' A dummy dataset of multi-response (select all that apply) data concerning
#' allergies of students at local high schools.
#'
#' @format A data frame with 400 rows of multi-response allergy information. If no allergies, the item is a zero-length character vector.
#' \describe{
#'   \item{allergies}{Allergy lists (list of factors)}
#'   \item{school}{High School: Brighton High, Cottonwood High, Olympus High, and Alta High (factor)}
#' }
#' @source Dummy data, invented by the developer
"allergies_by_school"

#' University Student Data
#'
#' A dummy dataset of various data types covering student profiles at well-known
#' universities: UCLA, Berkeley, Stanford, Yale, MIT, Texas Tech, and Alabama.
#'
#' @format A data frame with 3500 rows of student profile data.
#' \describe{
#'   \item{university}{The university name (factor)}
#'   \item{athletics}{If the student is involved in athletics (logical)}
#'   \item{height}{Height of the student in cm (numeric)}
#'   \item{semester_credits}{Credits enrolled in the current semester (integer)}
#'   \item{department}{Major department category (factor)}
#'   \item{uni_perception}{How this student feels about the university (factor)}
#'   \item{prof_support}{How supportive the students feel their average professor is (factor)}
#'   \item{food_court_per_week}{Which restaurants in the food courts they've visited this week}
#' }
#' @source Dummy data, invented by the developer
"students"
