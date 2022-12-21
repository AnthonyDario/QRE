An implementation of CRAs in Haskell as described in "Streamable Regular
Transductions" (Alur et al.). Used to execute StreamQREs

## Todo
- Don't think I need the tags in the theta function since they are already
  associated with a tag
- Allow the specification of the wildcard tag
- Remove the need to specify the start state?
- Generically typed CRA's
- Showing CRAs better
    - RegUpdate
    - F
    - I
- Make CRAs run with an infinite stream
    - Stream -> Stream
- Implement StreamQRE -> CRA 
    - Empty
    - Identity function
    - Iter
    - Split
    - Choice
