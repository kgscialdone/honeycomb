# Honeycomb Documentation

This branch contains the Github Pages documentation for Honeycomb. It is controlled by an [automated workflow](https://github.com/KatrinaKitten/honeycomb/actions/workflows/gendocs.yml) which updates it on each push to the `master` branch. It should not be updated manually unless absolutely necessary; branches created from `master` have their `docs` folder `.gitignore`d to allow for local copies during testing, without polluting the `master` branch with generated documentation.
