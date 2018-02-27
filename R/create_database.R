#' Creating database
#'
#'  Function \code{create_database} creates a dabase
#'  with
#'
#' @export
#' @import RMySQL
#'


create_database <- function(dbname, user, password, host) {

  stopifnot(is.character(dbname), is.character(user),
            is.character(password), is.character(host))

  database<- dbConnect(MySQL(),dbname = dbname,
                    user = user,
                    password = password,
                    host = host)
  # set encoding of connection
  dbSendQuery(database,"SET NAMES 'utf8';")

  dbSendQuery(database,"DROP TABLE IF EXISTS statements;")
  dbSendQuery(database, "DROP TABLE IF EXISTS national_party;")
  dbSendQuery(database, "DROP TABLE IF EXISTS eu_party;")
  dbSendQuery(database, "DROP TABLE IF EXISTS eu_party_code;")
  dbSendQuery(database,"DROP TABLE IF EXISTS languages;")
  dbSendQuery(database,"DROP TABLE IF EXISTS term_of_office;")
  dbSendQuery(database,"DROP TABLE IF EXISTS deputies;")
  # creating table with deputies

  dbSendQuery(database,'
              CREATE TABLE deputies (
              id VARCHAR(20) NOT NULL,
              name VARCHAR(50),
              link VARCHAR(200),
              nationality VARCHAR(20),
              date_of_birth DATE,
              place_of_birth VARCHAR(50),
              date_of_death DATE,
              PRIMARY KEY (id))')

  # cretaing table with term of offices of deputies

  dbSendQuery(database,'
              CREATE TABLE term_of_office (
              deputies_id VARCHAR(20) NOT NULL,
              term VARCHAR(3) NOT NULL,
              PRIMARY KEY (deputies_id, term),
              FOREIGN KEY (deputies_id) REFERENCES deputies(id)
              )')

  # creating table with languages

  dbSendQuery(database,'
              CREATE TABLE languages (
              id VARCHAR(3) NOT NULL,
              full_name VARCHAR(20),
              PRIMARY KEY (id))')

  # creating table with eu_partycodes

  dbSendQuery(database, '
              CREATE TABLE eu_party_code (
              id VARCHAR(10) NOT NULL,
              full_name VARCHAR(88),
              PRIMARY KEY (id))')


  # creating table with eu_party history of deputies

  dbSendQuery(database, '
              CREATE TABLE eu_party(
              id INT AUTO_INCREMENT NOT NULL,
              date_beginning DATE,
              date_end DATE,
              deputies_id VARCHAR(20),
              position VARCHAR(50),
              full_name VARCHAR(200),
              original_text VARCHAR(210),
              PRIMARY KEY (id),
              FOREIGN KEY (deputies_id) REFERENCES deputies(id)
              )')

  # creating table with national parties history of deputies

  dbSendQuery(database, '
              CREATE TABLE national_party (
              id INT AUTO_INCREMENT,
              full_name VARCHAR(100),
              date_beginning DATE,
              date_end DATE,
              deputies_id VARCHAR(20),
              PRIMARY KEY (id),
              FOREIGN KEY (deputies_id) REFERENCES deputies(id)
              )')

  # creating table with statements

  dbSendQuery(database,'
              CREATE TABLE statements (
              id INT AUTO_INCREMENT,
              deputies_id VARCHAR(20) NOT NULL,
              date DATE,
              title TEXT,
              reference VARCHAR(40),
              language_code VARCHAR(2),
              text TEXT,
              duration INT,
              start_time DATETIME,
              end_time DATETIME,
              term VARCHAR(3),
              link VARCHAR(200),
              PRIMARY KEY (id),
              FOREIGN KEY (deputies_id) REFERENCES deputies(id),
              FOREIGN KEY (language_code) REFERENCES languages(id)
              )')

  # disconnecting from database
  suppressWarnings(database)

}

