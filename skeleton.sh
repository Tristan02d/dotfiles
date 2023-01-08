#!/bin/bash

squelette_C(){
echo "/**
*  @filename $filename
*  @date $(date)
*  @author Tristan Riehs <triehs@enseirb-matmeca.fr>
*  @brief ...
*/

#include <stdlib.h>
#include <stdio.h>

int main( int argc, char *argv[] )
{

  return EXIT_SUCCESS;
}" > $file_fullname
}

squelette_sh(){
echo "#
#  @filename $filename
#  @date $(date)
#  @author Tristan Riehs <triehs@enseirb-matmeca.fr>
#  @brief ...
#
#!/bin/bash" > $file_fullname
}

squelette_tex(){
echo "%%
%% @filename $filename
%% @date $(date)
%% @author Tristan Riehs <triehs@enseirb-matmeca.fr>
%% @brief ...
%%
\documentclass[a4paper, draft]{article}

\usepackage[utf8]{inputenc}
\usepackage[french]{babel} 

% Figures
\usepackage{graphicx}
\graphicspath{{./images/}}

% Math
\usepackage{amsmath, amssymb}
\newtheorem{defi}{Définition}

% Algortihmes
\usepackage[vlined,lined,linesnumbered,boxed,french]{algorithm2e}
\DeclareMathOperator*{\argmin}{argmin}
\DeclareMathOperator{\myfunc}{myfunc}
\DeclareMathOperator*{\sign}{sign}
\DeclareMathOperator*{\imwh}{width}
\DeclareMathOperator*{\imht}{height}

% Extra
\usepackage[left=3cm,right=3cm,top=2cm,bottom=2cm]{geometry}
\usepackage{url}

\begin{document}


\end{document}" > $file_fullname
}

#Vérification des arguments
if [ $# -ne 1 ]
then
    echo "Il faut le nom du fichier à créer avec la bonne extension en paramètre:
script bash    -> .sh
programme C    -> .c
header C       -> .h
document LaTeX -> .tex"
    exit
fi

# Début du code principal
file_fullname=$1

touch $file_fullname
chmod u+wrx $file_fullname

echo $file_fullname > $file_fullname

extension=$(rev $file_fullname |cut -f1 -d'.' | rev)
filename=$(rev $file_fullname | cut -f1 -d'/' | rev)

# Récupération des noms des auteurs
authors="Tristan Riehs e"

echo "Y a-t-il d'autres auteurs?
Si non, laisser vide.
Si oui, écrire et entrer leurs noms un par un au format
Prénom Nom adresse_email
si l'adresse emai est une adresse de l'ENSEIRB, mettre \"e\"."
read new_author

while [ "$new_author" ]
do
    authors+="
$new_author"
    read new_author
done
echo "Liste des auteurs:
$authors"

# Attribution du bon squelette
if [ "$extension" = 'c' ] || [ "$extension" = 'h' ]
then
squelette_C
fi

if [ "$extension" = 'tex' ]
then
squelette_tex
fi

if [ "$extension" = 'sh' ]
then
squelette_sh
fi
