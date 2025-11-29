-- Create user_metadata table to hold extended user profile data
CREATE TABLE user_metadata
  ( id SERIAL8 PRIMARY KEY UNIQUE
  , user_id INT8 NOT NULL UNIQUE REFERENCES users(id) ON DELETE CASCADE
  , display_name VARCHAR NOT NULL
  , full_name VARCHAR NOT NULL
  , avatar_url VARCHAR
  , is_admin BOOL NOT NULL DEFAULT FALSE
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
  , updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
  );

CREATE INDEX idx_user_metadata_user_id ON user_metadata(user_id);

-- Migrate existing data from users table
INSERT INTO user_metadata (user_id, display_name, full_name, avatar_url, is_admin)
SELECT id, display_name, full_name, avatar_url, is_admin FROM users;

-- Remove columns from users table
ALTER TABLE users DROP COLUMN display_name;
ALTER TABLE users DROP COLUMN full_name;
ALTER TABLE users DROP COLUMN avatar_url;
ALTER TABLE users DROP COLUMN is_admin;
