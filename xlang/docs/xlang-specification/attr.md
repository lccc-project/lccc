# Attributes


## Standard Attributes


### reify_as_transparent_if_field_1zst

Impl Note: This attribute is binding on the implementation

This attribute shall appear only on a field declaration of a struct which has exactly one other field which does not have this attribute. 

During type reification, if the field this attribute appears on has a sized type size 0 and alignment 1, then the enclosing structure shall be reified as the type of the field without this attribute. 

### mutable

Impl Note: This attribute is binding on the implementation

This attribute shall appear only on a field declaration. 
