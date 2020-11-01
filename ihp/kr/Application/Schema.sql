CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE TABLE wastecollections (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL
);
