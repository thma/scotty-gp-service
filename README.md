# scotty-gp-service
using scotty and generic-persistence to build a database backed REST service in Haskell

## Features demontrated

- REST API with Scotty routes
- Database access with generic-persistence
- Pagination of results with `page`and `size` query parameters backed by generic-persistence queries
- Request Logging with `logStdoutDev` WAI middleware
- Access Authorization with token based authentication using `wai-middleware-bearer` middleware

