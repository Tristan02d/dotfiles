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

\author{Tristan Riehs}
\title{}

\begin{document}
\maketitle

\end{document}" > $file_fullname
}

prompt_images_dir(){
    echo "Voulez vous créer le dossier $file_dirname/images ? [o/N]"
    read answer
    if [ "$answer" = "o" ] || [ "$answer" = "O" ]
    then
	mkdir $file_dirname/images
    elif [ "$answer" ] && [ "$answer" != "n" ] && [ "$answer" != "N" ]
    then
	prompt_images_dir
    fi
}

#Vérification des arguments
if [ $# -ne 1 ]
then
    echo "Il faut spécifier le nom du fichier à créer avec la bonne extension en paramètre:
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

extension=$(rev $file_fullname | cut -f1 -d'.' | rev)
filename=$(rev $file_fullname | cut -f1 -d'/' | rev)
file_dirname=$(dirname $file_fullname)

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
    prompt_images_dir
fi

if [ "$extension" = 'sh' ]
then
    squelette_sh
fi
