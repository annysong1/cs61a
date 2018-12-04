CREATE TABLE parents AS
  SELECT "abraham" AS parent, "barack" AS child UNION
  SELECT "abraham"          , "clinton"         UNION
  SELECT "delano"           , "herbert"         UNION
  SELECT "fillmore"         , "abraham"         UNION
  SELECT "fillmore"         , "delano"          UNION
  SELECT "fillmore"         , "grover"          UNION
  SELECT "eisenhower"       , "fillmore";

CREATE TABLE dogs AS
  SELECT "abraham" AS name, "long" AS fur, 26 AS height UNION
  SELECT "barack"         , "short"      , 52           UNION
  SELECT "clinton"        , "long"       , 47           UNION
  SELECT "delano"         , "long"       , 46           UNION
  SELECT "eisenhower"     , "short"      , 35           UNION
  SELECT "fillmore"       , "curly"      , 32           UNION
  SELECT "grover"         , "short"      , 28           UNION
  SELECT "herbert"        , "curly"      , 31;

CREATE TABLE sizes AS
  SELECT "toy" AS size, 24 AS min, 28 AS max UNION
  SELECT "mini"       , 28       , 35        UNION
  SELECT "medium"     , 35       , 45        UNION
  SELECT "standard"   , 45       , 60;

-------------------------------------------------------------
-- PLEASE DO NOT CHANGE ANY SQL STATEMENTS ABOVE THIS LINE --
-------------------------------------------------------------

-- Q1 --
-- The size of each dog
CREATE TABLE size_of_dogs AS
  SELECT a.name, b.size FROM dogs AS a, sizes AS b WHERE a.height > b.min and a.height <= b.max;

-- Q2 --
-- All dogs with parents ordered by decreasing height of their parent
CREATE TABLE by_parent_height AS
  SELECT child FROM dogs AS a, parents AS b WHERE a.name = b.parent ORDER BY -a.height;

-- Q3 --
-- Filling out this helper table is optional
CREATE TABLE siblings AS
  SELECT "REPLACE THIS LINE WITH YOUR SOLUTION";

-- Sentences about siblings that are the same size
CREATE TABLE sentences AS
  WITH 
  siblings_size (child1, child2, size) AS (
    select a.child, b.child, c.size from parents as a, parents as b, size_of_dogs as c, size_of_dogs as d
      where a.parent = b.parent and a.child < b.child and a.child = c.name and b.child = d.name and c.size = d.size
  )
  select child1 || ' and ' || child2 || ' are ' || size || ' siblings' from siblings_size;

-- Q4 --
-- Ways to stack 4 dogs to a height of at least 170, ordered by total height
CREATE TABLE stacks_helper(dogs, stack_height, last_height);
-- Add your INSERT INTOs here
CREATE TABLE stacks AS
  WITH 
    stacks_dog (names, n, dog_height, total_height) AS (
      select name, 1, height, height from dogs union
      select a.name || ", " || b.names, b.n+1, a.height, a.height + b.total_height 
        from dogs as a, stacks_dog as b
        where b.n < 4 and a.height < b.dog_height
      )
  select names, total_height from stacks_dog where n = 4 and total_height >= 170 order by total_height;