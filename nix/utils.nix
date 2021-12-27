with builtins;
let 
  isPrivate = str: substring 0 2 str == "__"; 
  removePrivate = set: removeAttrs set (filter isPrivate (attrNames set));
  toUrl = x: "https://github.com/" + x.owner + "/" + x.repo;
  toUrlGit = x: toUrl x + ".git";
  toRevSha = x: listToAttrs [{ name = x.rev; value = x.sha256;  }];  
  toShaItem = x: [ # We use two versions of path because sometimes they write it with .git at the end sometimes they skip that.
    { name = toUrl x   ; value = toRevSha x; }
    { name = toUrlGit x; value = toRevSha x; }
    ];
in
  { 
    # Reads sha256map from niv sources file.
    getSha256Map = sources: listToAttrs (concatLists 
      (map toShaItem (attrValues (removePrivate sources))));
  }
