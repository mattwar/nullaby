# nullaby
A null checking Roslyn diagnostic analyzer for C#

This is a prototype used to explore the feasability of using a Roslyn code analyzer to perform 
checking for improper dereferencing of potentially null variables and improper assignment
of potentially null values to variables that are not meant to be null.

It does not handle all possible cases and only considers code flow in a lexical sense (top to bottom).

It relies on the existence of custom attributes marking fields, parameters and return types to denote intent.
Variables not declared with an attribute are not considered for null checking, unless through analysis they are
shown to be in a known null or not-null state.

The specific attributes are `CouldBeNullAttribute` and `ShouldNotBeNullAttribute`. They are not defined by the analyzer,
but the analyzer will respond to any attributes with these names.

