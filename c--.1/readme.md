# Compilateur C-- Mohamed HADJOUDJ

La partie 1 du compilateur a été entièrement refaite, pour plus de lisibilité et pour que le code soit plus simple à maintenir.

J'ai aussi ajouté quelques commentaires dans le code afin d'en faciliter la compréhension, particulierement dans le code assembleur généré.

## Partie 2

L'idée de base pour gérer les exceptions à travers plusieurs appel de fonctions, a été de copier un systeme souvent utilisée par les codeurs ocaml ou rust, qui est de renvoyer:

```ocaml
Error(...arguments)
```
ou 
```ocaml
Ok(...arguments)
```

Concretement on si on veut renvoyer un résultat on écrit 0 dans %rax et la valeur du resultat dans rbx, si on veut renvoyer une exception on met un nombre non nul dans %rax (par exemple 1), l'identifiant de l'exception (j'ai décidé de remplacer les noms des exceptions par des nombres, pour eviter l'utilisation de strcmp) dans %rbx, et le parametre de l'exception dans %rcx.

Il manquait alors à gerer le cas où l'on retourne un résultat ou que l'on lance une exception avant un finally.
Pour cela j'ai crée quatre variables globales:
```
.MCC_RET_FINALLY
.MCC_RET_FINALLY_RAX
.MCC_RET_FINALLY_RBX
.MCC_RET_FINALLY_RCX
```
La première est non nulle si et seulement si il y a un return à faire après l'execution du code de finally, les autres contiennent les contenus des registres %rax, %rbx et %rcx. Après l'execution du code de finally, on verifie s'il y a quelque chose à retourner et si c'est le cas on met à jour %rax, %rbx et %rcx et on "return".

