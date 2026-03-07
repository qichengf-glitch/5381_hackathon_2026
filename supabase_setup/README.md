# Supabase Setup

1. Copy `.env.example` to project root as `.env`.
2. Fill in:
   - `SUPABASE_URL`
   - `SUPABASE_ANON_KEY`
   - `SUPABASE_SERVICE_ROLE_KEY`
3. Run:

```r
source("/Users/qichengfu/Desktop/5381_hackathon/supabase_setup/supabase_fetch.R")
```

Notes:
- Use `SERVICE_ROLE_KEY` only in backend/server code.
- Never expose `SERVICE_ROLE_KEY` in frontend.
