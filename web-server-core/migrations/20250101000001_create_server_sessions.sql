CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE server_sessions
  ( id UUID PRIMARY KEY NOT NULL DEFAULT uuid_generate_v1mc ()
  , user_id SERIAL8 NOT NULL REFERENCES users (id) ON DELETE CASCADE
  , ip_address inet
  , user_agent text
  , expires_at timestamptz NOT NULL
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
  , updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

