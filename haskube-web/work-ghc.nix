{ reflex-platform, ... }: reflex-platform.ghc.override {
  overrides = self: super: {
    haskube = self.callPackage ../haskube {};
  };
}