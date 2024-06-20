# Svenski: Din Anki-assistent för att Lära Dig Svenska!

## Overview

Svenski is a Shiny app designed to assist users in learning Swedish by integrating various features like translation, text-to-speech, image generation, and Anki card creation. This app provides a comprehensive and interactive way to learn and practice Swedish.

## Features

- **Translation**: Translate text between Swedish and English using a free translation API.
- **Text Analysis**: Analyze Swedish text to provide detailed information such as meaning, part of speech, and example sentences.
- **Audio Generation**: Generate audio for example sentences using the OpenAI TTS API.
- **Image Generation**: Create images based on example sentences using DALL-E.
- **Anki Integration**: Create Anki flashcards with detailed information and media to assist in learning.

## Screenshots

### Translation Feature
![Translation Feature](readme_pics/Skärmavbild%202024-06-20%20kl.%2013.09.02.png)

### Text Analysis Results
![Text Analysis Results](readme_pics/Skärmavbild%202024-06-20%20kl.%2013.09.20.png)

### Generated Audio and Picture
![Generated Audio and Picture](readme_pics/Skärmavbild%202024-06-20%20kl.%2013.09.34.png)

### Anki Card Creation
![Anki Card Creation](readme_pics/Skärmavbild%202024-06-20%20kl.%2013.10.55.png)

## Installation

To run the Svenski app locally, follow these steps:

1. **Clone the repository**:
    ```bash
    git clone https://github.com/rasools/svenski.git
    cd svenski
    ```

2. **Install the required packages**:
    ```R
    install.packages(c("shiny", "reticulate", "spacyr", "DT", "httr", "jsonlite", "base64enc", "magick", "shinythemes", "shinyjs"))
    ```

3. **Set up Python environment**:
    - Install [Miniconda](https://docs.conda.io/en/latest/miniconda.html).
    - Create a conda environment and install the required Python packages:
        ```bash
        conda create -n svenski python=3.8
        conda activate svenski
        pip install spacy
        python -m spacy download sv_core_news_md
        ```

4. **Run the app**:
    ```R
    library(shiny)
    runApp('svenski_shiny_imp9.R')
    ```

## Usage

1. **Translate Text**: Enter the text you want to translate, select the source and target languages, and click the "Translate" button to see the translation.
2. **Submit a Swedish Word/Phrase**: Enter a Swedish word or phrase and click "Submit" to analyze the text and generate detailed information.
3. **Generate Audio**: Click the "Generate Audio" button to create audio files for example sentences.
4. **Generate Picture**: Click the "Generate Picture" button to create an image based on an example sentence.
5. **Create Anki Card**: Click the "Create Anki Card" button to generate an Anki card with the analyzed information and media.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request or open an issue to discuss any changes.

## License

This project is licensed under the MIT License.

## Acknowledgements

- [OpenAI](https://www.openai.com/) for the APIs used in the app.
- [spaCy](https://spacy.io/) for text analysis.
- [Shiny](https://shiny.rstudio.com/) for the web framework.
