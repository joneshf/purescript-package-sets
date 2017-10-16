BEGIN TRANSACTION;

CREATE TABLE IF NOT EXISTS package (
  name TEXT,
  package_set TEXT,
  repo TEXT,
  version TEXT,
  PRIMARY KEY (name, package_set)
);

CREATE TABLE IF NOT EXISTS dependencies (
  independent TEXT REFERENCES package (name) ON DELETE CASCADE ON UPDATE CASCADE,
  dependent TEXT REFERENCES pacakge (name) ON DELETE CASCADE ON UPDATE CASCADE,
  package_set TEXT REFERENCES pacakge (package_set) ON DELETE CASCADE ON UPDATE CASCADE,
  UNIQUE (independent, dependent, package_set)
);

COMMIT;
