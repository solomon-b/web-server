CREATE TABLE server_sessions
  ( id UUID PRIMARY KEY NOT NULL DEFAULT gen_random_uuid()
  , user_id SERIAL8 NOT NULL REFERENCES users (id) ON DELETE CASCADE
  , ip_address inet
  , user_agent text
  , expires_at timestamptz NOT NULL
  , last_activity_at timestamptz NOT NULL DEFAULT now()
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
  , updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

