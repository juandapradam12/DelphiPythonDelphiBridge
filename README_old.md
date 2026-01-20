# Python-Delphi Bridge Template

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Delphi](https://img.shields.io/badge/Delphi-12-red.svg)](https://www.embarcadero.com/products/delphi)
[![Python](https://img.shields.io/badge/Python-3.11-blue.svg)](https://www.python.org/downloads/)

A production-ready **template repository** for embedding Python into **Delphi VCL** applications using [Python4Delphi (P4D)](https://github.com/pyscripter/python4delphi) and the **Python 3.11 embeddable runtime**.

## 🎯 Overview

This template provides a complete foundation for Python-Delphi integration:
- **Delphi Application**: Manages and embeds the Python runtime (embeddable distribution)
- **Python Bridge**: Executes data processing logic and returns structured JSON
- **Seamless Integration**: Type-safe communication between Delphi and Python
- **Production Ready**: Comprehensive configuration, documentation, and team workflows

> 🚀 **Use this repository as a template** for any Python-Delphi integration project

## ✨ Features

- 🔧 **Complete Setup**: Pre-configured submodules for Python4Delphi and embedded Python
- 📦 **Embeddable Python**: Includes Python 3.11.9 embedded distribution as submodule
- 🛠️ **VS Code Integration**: Optimized workspace settings for multi-language development
- 📋 **Team Workflows**: Comprehensive documentation and contribution guidelines
- 🔒 **Security**: Security policy and best practices for production deployment
- 📊 **Data Processing**: Example implementation with pandas DataFrame processing
- 🎨 **Clean Architecture**: Organized project structure with separation of concerns

---

## 📂 Project Structure

```text
├── README.md                   # Comprehensive documentation (all-in-one)
├── .gitignore                  # Comprehensive ignore patterns
├── app_delphi/                 # Delphi VCL application
│   ├── DelphiApp.dpr          # Main project file
│   ├── DelphiApp.dproj        # Delphi project configuration
│   ├── forms/                 # Application forms
│   │   └── MainForm.pas       # Main UI form
│   └── services/              # Python integration services
│       └── PyEngineService.pas # Python engine wrapper
├── app_py/                     # Python processing modules
│   ├── main.py                # Python entry point
│   └── requirements.txt       # Python dependencies
├── data/                       # Sample data files
│   └── input/                 # Input data examples
├── external_libraries/         # Git submodules
│   ├── python4delphi/         # P4D components (submodule)
│   └── python-3.11.9-embed-amd64/ # Embedded Python (submodule)
└── .vscode/                    # VS Code workspace configuration
    └── settings.json          # IDE settings
```

---

## 🚀 Quick Start

### 1. Create New Project from Template

#### Option A: GitHub Template (Recommended)
1. Click **"Use this template"** button on GitHub
2. Create your new repository
3. Clone with submodules:
```bash
git clone --recurse-submodules https://github.com/yourusername/your-new-repo.git
cd your-new-repo
```

#### Option B: Manual Clone
```bash
git clone --recurse-submodules https://github.com/juandapradam12/PythonDelphiPOC.git YOUR_PROJECT
cd YOUR_PROJECT
git remote remove origin
git remote add origin https://github.com/YOUR_USERNAME/YOUR_NEW_REPO.git
git push -u origin main
```

### 2. Environment Setup

#### Verify Submodules
```bash
# Check submodule status
git submodule status

# Initialize if needed
git submodule update --init --recursive
```

#### Install Python Dependencies
```bash
cd external_libraries/python-3.11.9-embed-amd64
.\python.exe -m pip install -r ..\..\app_py\requirements.txt
```

#### Configure Delphi
1. Open `app_delphi\DelphiApp.dproj` in Delphi IDE
2. Set **Win64** as active platform
3. Add to Library Path:
   - `external_libraries\python4delphi\Source`
   - `external_libraries\python4delphi\Source\vcl`
4. Build and run the application

### 3. Test the Integration

1. Run the Delphi application
2. Enter a file path in the memo (or leave empty for default)
3. Click **"Run Python Processing"**
4. View JSON results returned from Python

## 📋 Example Output

Input file: `data/input/sample_points.txt`

```json
{
  "status": "ok",
  "path": "C:\\YourProject\\data\\input\\sample_points.txt",
  "rows": 3,
  "cols": 3,
  "columns": ["x", "y", "z"],
  "first_row": {"x": 0.1, "y": 1.2, "z": 2.3}
}
```

---

## 🏗️ Architecture & Development

### Communication Flow
1. **Delphi → Python**: Passes file paths and parameters
2. **Python Processing**: Executes data logic using pandas/numpy
3. **Python → Delphi**: Returns structured JSON results
4. **Delphi Display**: Parses and displays results in UI

### Key Components
- **PyEngineService**: Delphi service that manages Python runtime
- **MainForm**: VCL form providing user interface
- **main.py**: Python entry point for data processing
- **Submodules**: External dependencies managed as Git submodules

### Development Workflow

#### Daily Development
```bash
# Pull latest changes (including submodules)
git pull --recurse-submodules

# Make changes, test thoroughly
# ...

# Commit and push
git add .
git commit -m "Descriptive commit message"
git push
```

#### Feature Development
```bash
# Create feature branch
git checkout -b feature/your-feature-name

# Work on feature, commit changes
git add .
git commit -m "feat: implement feature X"
git push -u origin feature/your-feature-name

# Create pull request via GitHub
```

### Submodule Management

#### Update Python4Delphi
```bash
git submodule update --remote external_libraries/python4delphi
git add external_libraries/python4delphi
git commit -m "Update Python4Delphi to latest version"
```

#### Update Python Distribution
```bash
git submodule update --remote external_libraries/python-3.11.9-embed-amd64
git add external_libraries/python-3.11.9-embed-amd64
git commit -m "Update Python embedded distribution"
```

#### After Team Updates
```bash
# After pulling, update submodules
git submodule update --init --recursive
```

---

## 🔧 Configuration & Requirements

### Environment Requirements
- **Delphi**: Community Edition or higher (tested with Delphi 12)
- **Python**: 3.11.9 (included as embedded distribution)
- **Git**: For submodule management
- **VS Code**: Recommended for Python development (optional)

### Testing Guidelines

#### Python Testing
```bash
cd app_py
..\external_libraries\python-3.11.9-embed-amd64\python.exe -m pytest tests/
```

#### Delphi Testing
- Use DUnit or DUnitX for unit tests
- Test Python integration thoroughly
- Verify error handling for Python script failures

### Troubleshooting

#### Common Issues
1. **Submodules not initialized**: `git submodule update --init --recursive`
2. **Python import errors**: Check `requirements.txt` and Python path
3. **Delphi compilation errors**: Verify library paths include P4D sources
4. **VS Code showing many changes**: Reload window, check `.vscode/settings.json`

---

## 🤝 Contributing & Standards

### Code Standards

#### Delphi Code
- Use meaningful variable and function names
- Include XML documentation comments for public methods
- Follow Object Pascal naming conventions
- Keep unit dependencies minimal and well-organized

#### Python Code
- Follow PEP 8 style guidelines
- Use type hints where appropriate
- Include docstrings for all public functions and classes
- Write unit tests for new functionality

### Contribution Workflow

1. **Fork the Repository**: Create a fork in your GitHub account
2. **Clone and Setup**: 
   ```bash
   git clone https://github.com/yourusername/PythonDelphiPOC.git
   git submodule update --init --recursive
   ```
3. **Create Feature Branch**: `git checkout -b feature/your-feature-name`
4. **Make Changes**: Follow coding standards, test thoroughly
5. **Submit Pull Request**: Include summary, testing notes, and screenshots

### Commit Message Format
```
type(scope): description

[optional body]
[optional footer]
```

**Types**: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`

**Examples**:
- `feat(delphi): add new Python method callback system`
- `fix(python): resolve memory leak in data processing`
- `docs(readme): update installation instructions`

---

## 🔒 Security & Best Practices

### Security Considerations

#### Python Integration Security
- Keep embedded Python distribution updated
- Validate all Python code execution paths
- Sanitize data passed between Delphi and Python
- Use appropriate error handling to prevent information disclosure

#### Delphi Application Security
- Follow secure coding practices for Windows applications
- Validate all user inputs
- Implement proper access controls
- Handle sensitive data appropriately

#### Development Best Practices
1. **Input Validation**: Always validate data passed between Python and Delphi
2. **Error Handling**: Implement comprehensive error handling
3. **Logging**: Secure logging practices (avoid logging sensitive data)
4. **Updates**: Keep all dependencies updated to latest secure versions
5. **Testing**: Include security testing in development process

### Deployment Considerations

#### Python Dependencies
- Keep `requirements.txt` updated
- Consider pinning versions for production:
```txt
pandas==2.1.0
numpy==1.24.3
```

#### Delphi Deployment
- Include Python embedded distribution in deployment
- Test on target machines without Python installed
- Include necessary Visual C++ redistributables

### Reporting Security Issues

**⚠️ Do NOT create public issues for security vulnerabilities**

For security issues, contact repository maintainers directly with:
- Description of the vulnerability
- Steps to reproduce
- Potential impact assessment
- Suggested fix (if available)

---

## 📦 Customization & Template Usage

### For New Projects

#### Checklist
- [ ] Repository created from template
- [ ] Submodules initialized and working
- [ ] Python dependencies installed
- [ ] Delphi project compiles and runs
- [ ] Project-specific documentation updated
- [ ] Team access configured
- [ ] Development branch protection rules set

#### Customization
- Modify `.gitignore` to customize ignore patterns for your project
- Modify `app_py/requirements.txt` for Python dependencies
- Update project structure as needed
- Customize this README for your specific project

### Best Practices for Teams
1. **Never commit build outputs** - They're ignored by default
2. **Keep submodules updated** - But test thoroughly
3. **Use descriptive commit messages** - Help your future self
4. **Test Python changes** - Both standalone and integrated
5. **Document breaking changes** - Especially in Python interfaces
6. **Review code changes** - Use pull requests
7. **Keep dependencies minimal** - Only add what you really need
8. **Version your releases** - Tag stable versions

---

## 🆘 Support & Issues

### Getting Help
- 🐛 **Bug Reports**: Create a new issue with detailed reproduction steps
- 💡 **Feature Requests**: Create a new issue describing your proposed feature
- ❓ **Questions**: Create an issue with the "question" label
- 📚 **Documentation**: Check this comprehensive guide
- 🔗 **External Resources**: [Python4Delphi documentation](https://github.com/pyscripter/python4delphi)

### Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| Latest  | :white_check_mark: |

---

## 🎯 Roadmap

- [ ] Additional Python processing examples
- [ ] Multi-threading support
- [ ] Enhanced error handling and logging
- [ ] Performance optimization guides
- [ ] Docker containerization option
- [ ] CI/CD pipeline templates
- [ ] Cross-platform considerations

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

**Made with ❤️ for the Delphi and Python communities**

> This template consolidates all documentation into a single comprehensive guide. For the most up-to-date information and detailed examples, refer to the code in the repository.