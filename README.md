This project is starting as an exerpt and a refactoring of the MultiWorker portion of the Hacklang tooling.
The refactoring principle is using functors to abstract all implementation dependencies.

Wikipedia job to simulate type checking:
- The naming table is the index file
- Parse each page for links
- Compute the hash of this page based on the hashes of the linked pages
- The dependency table will contain <hash of page name> -> <hashes of dependent page names>

Tactical matters:
- The index is itself too large - need to read it gradually and terminate after N articles or T amount of time
- Need to build a naming table in memory or in SQLite
    - <article name> -> <offset in multistream>

Time on the version on commit dc59157 (the first version that created the reverse index for the entire index of Wikipedia):
```
dune exec src/demo.exe  226.51s user 32.93s system 98% cpu 4:24.11 total
```

Next: create a job to compute a hash of an article based on the hash of its text and its referenced dependencies