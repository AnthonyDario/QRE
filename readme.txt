An implementation of CRAs in Haskell as described in "Streamable Regular
Transductions" (Alur et al.). Used to execute StreamQREs

## Todo
- Remove the state and tags list
- Allow the specification of the wildcard tag
- Don't think I need the tags in the theta function since they are already
  associated with a tag
- Remove the need to specify the start state?
- Generically typed CRA's
- Showing CRAs better
- Implement StreamQRE -> CRA 
