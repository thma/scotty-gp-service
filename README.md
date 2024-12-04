# scotty-gp-service
Using scotty and generic-persistence to build a database backed REST service in Haskell

## Features demontrated

- REST API with Scotty routes
- Database access with generic-persistence
- Pagination of results with `page`and `size` query parameters backed by generic-persistence queries
- Request Logging with `logStdoutDev` WAI middleware
- Access Authorization with token based authentication using `wai-middleware-bearer` middleware

## Detailed Description

You can find a detailed description of the code in the following blog post: [Real World REST APIs with Scotty and Generic-Persistence](https://thma.github.io/posts/2024-12-05-real-worlds-rest-services-with-scotty-and-gp.html).



