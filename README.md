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

Next: create a job to compute a hash of an article based on the hash of its text and its referenced dependencies'

IDEA: instead of computing hash, look for missing articles

Next: run shallow checker in MultiWorker (will need to use shared memory)

Next: double-check if we're actually caching the offsets and the articles. I think not.
Try different size jobs.

Test of 500 work items:

SharedMemXmlCatalog:
Telemetry:
    Total time: 48.4398369789
    Time in catalog: 383.6317710876
    Time in index: 64.0354182720
    Cache time: 0.7987151146
    Other time: 0.0281863213
    Cache hits: 138083
    Cache misses: 84328

HashTableXmlCatalog:
Telemetry:
    Total time: 67.0519320965
    Time in catalog: 558.8913812637
    Time in index: 61.3832283020
    Cache time: 1.1174175739
    Other time: 0.0198464394
    Cache hits: 74492
    Cache misses: 84073