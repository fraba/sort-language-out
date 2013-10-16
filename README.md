sort-language-out
=================

R script to identify language of string

This simple script labels and removes messages made in a different language. 
In this case the languages are English and Italian but it is possible to use any 
combination of languages.

The case scenario is a forum where comments in English are treated as spam while comments
in Italian are considered ham. The script labels string based on their probable language 
and then delete comments in one specific language (in this case English). 
You can of course easily tweak the script so to manipulate the messages once labelled
in the most appropriate way depending on your needs. 
