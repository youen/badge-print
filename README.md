# 🖨️ Badge Print

Un générateur de badges professionnel, simple et élégant, conçu pour l'impression rapide. Entièrement développé en **Elm** avec **Tailwind CSS**.

![Badge Print Preview](https://raw.githubusercontent.com/youen/badge-print/main/preview.png) *(Note: Remplacez par une vraie capture d'écran une fois publié)*

## ✨ Fonctionnalités

- 🖼️ **Import de logo** : Intégrez votre logo d'entreprise ou d'événement.
- 📝 **Saisie flexible** : Support des listes de noms avec séparateur personnalisable (Espace, Virgule, Point-virgule).
- 🎨 **Personnalisation avancée** :
  - Position verticale du texte ajustable.
  - Opacité du logo contrôlable.
  - Marge du logo paramétrable.
  - Option de fond blanc sous le nom pour une lisibilité maximale.
- 📐 **Formats multiples** : Support des formats standards (85x55mm, 90x60mm) et A6.
- ↕️ **Orientation** : Basculez entre les modes Paysage et Portrait.
- 🖨️ **Optimisé pour l'impression** : Génération de planches A4 avec repères de coupe.

## 🚀 Installation & Développement

Le projet utilise **Elm** et **Tailwind CSS**.

### Prérequis

- [Elm 0.19.1+](https://guide.elm-lang.org/install/elm.html)
- [Node.js & npm](https://nodejs.org/)

### Installation

```bash
git clone https://github.com/youen/badge-print.git
cd badge-print
npm install
```

### Build

Pour compiler l'application et générer le CSS :

```bash
# Compiler Elm
npx elm make src/Main.elm --output=main.js

# Générer CSS avec Tailwind v4
npx tailwindcss -i src/style.css -o style.css
```


## 📄 Licence

Ce projet est sous licence **MIT**. Voir le fichier [LICENSE](LICENSE) pour plus de détails.

Copyright © 2026 **Youen Péron**
