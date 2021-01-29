create extension pgcrypto;

create table items (
                          id bigserial primary key not null,
                          slug text not null unique,
                          name text not null,
                          description text not null,
                          category text not null,
                          price float not null,
                          image text not null,
                          created_at timestamptz not null,
                          updated_at timestamptz not null
);

create table categories (
                       id bigserial primary key not null,
                       name text not null unique
);

INSERT INTO items
        (slug, name, description, category, price, image, created_at, updated_at)
VALUES
       ('milanesa-de-carne', 'Milanesa de carne', 'Rica mila de carne', 'principales', 200, 'image1', now(), now());