-define(CONNECTED, connected).
-define(LOGGED_IN, logged_in).
-define(DISCONNECTED,disconnected).

-record( session, {
                   account_id,
                   socket,
                   transport,
                   connector_pid,
                   role_id,
                   session_state = ?DISCONNECTED
                  } ).
