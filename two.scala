// import scalaz._, Scalaz._

// import matryoshka._

// object two extends Extras {

//   // CREATE TABLE IF NOT EXISTS city (
//   //     id integer NOT NULL,
//   //     name varchar NOT NULL,
//   //     countrycode character(3) NOT NULL,
//   //     district varchar NOT NULL,
//   //     population integer NOT NULL
//   // );

//   case class City[C](name: String, country: C, district: String, population: Int)

//   // CREATE TABLE IF NOT EXISTS country (
//   //     code character(3) NOT NULL,
//   //     name varchar NOT NULL,
//   //     continent varchar NOT NULL,
//   //     region varchar NOT NULL,
//   //     surfacearea real NOT NULL,
//   //     indepyear smallint,
//   //     population integer NOT NULL,
//   //     lifeexpectancy real,
//   //     gnp numeric(10,2),
//   //     gnpold numeric(10,2),
//   //     localname varchar NOT NULL,
//   //     governmentform varchar NOT NULL,
//   //     headofstate varchar,
//   //     capital integer,
//   //     code2 character(2) NOT NULL --,
//   //     -- TODO: we can do this with CREATE DOMAIN
//   //     -- CONSTRAINT country_continent_check CHECK ((((((((continent = 'Asia'::text) OR (continent = 'Europe'::text)) OR (continent = 'North America'::text)) OR (continent = 'Africa'::text)) OR (continent = 'Oceania'::text)) OR (continent = 'Antarctica'::text)) OR (continent = 'South America'::text)));
//   // );

//   case class Country[C, L](code: String, name: String, cities: List[C], languages: List[L])

//   // CREATE TABLE IF NOT EXISTS countrylanguage (
//   //     countrycode character(3) NOT NULL,
//   //     language varchar NOT NULL,
//   //     isofficial boolean NOT NULL,
//   //     percentage real NOT NULL
//   // );

//   case class Language[C](name: String, countries: List[C])  


//   type X = Language[Country[City[Unit], Unit]]

// }

