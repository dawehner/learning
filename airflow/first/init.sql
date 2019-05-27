CREATE SCHEMA test_data_src;

CREATE TABLE test_data_src.homes(
  Sell int,
  List int,
  Living int,
  Rooms int,
  Beds int,
  Baths int,
  Age int,
  Acres float,
  Taxes int
);

CREATE SCHEMA test_data_dest;

CREATE TABLE test_data_dest.homes(
  Sell int,
  List int,
  Living int,
  Rooms int,
  Beds int,
  Baths int,
  Age int,
  Acres float,
  Taxes int
);


COPY test_data_src.homes from '/home/airflow/homes.csv' CSV HEADER;

