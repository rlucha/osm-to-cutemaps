module DB.Migrations.Postcode2Mercator where

-- get all the postcodes from the DB and perform a degrees to meter
-- conversion and create a Point geometry out of it, add it to the postcode
