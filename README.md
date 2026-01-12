# Atari Breakout – x86 Assembly (8086)

A fully functional implementation of the classic Atari Breakout arcade game, developed in **8086 Assembly Language** .This project interfaces directly with hardware using BIOS and DOS interrupt services to manage graphics, real-time input, and sound.

## 📝 Project Overview
The objective is to control a paddle at the bottom of the screen to bounce a character-based ball and destroy four rows of bricks arranged at the top.

## 🚀 Key Features
* **Low-Level Rendering**: Uses BIOS/DOS interrupt services for all display output on the 80x25 text mode screen.
* **Physics-Based Gameplay**: Ball movement simulates physics with allowed angles of 45°.
* **Collision Detection**: Robust logic for detecting collisions between the ball and bricks, walls, and the paddle.
* **Interrupt-Driven Input**: Real-time paddle movement controlled via keyboard interrupts using the Left and Right arrow keys.
* **Audio Feedback**: PC speaker notification tones triggered via interrupts for hitting the paddle, breaking bricks, or losing a life.
* **User Interface**: Includes an "ATARI ARCADE" welcome screen with rules, a live score tracker, and a life management system.


## 🛠️ Technical Implementation

### 1. Memory & Graphics
* **Display**: Renders directly to the video memory buffer at `0xB800`.
* **Bricks**: 4 rows of bricks at the top, each represented by a visible character in different colors.
* **Paddle**: A blue bar positioned at the bottom row.
* **Ball**: A character-based ball that bounces across the playfield.

### 2. Interrupt Usage
* **Display (INT 10h)**: Used for setting video modes and rendering characters.
* **Keyboard (INT 16h)**: Used for non-blocking, real-time paddle control.
* **DOS (INT 21h)**: Used for program termination and string display.
* **Sound (PIT)**: Hardware ports used to trigger specific frequencies through the PC speaker.

### 3. Game Logic
* **Score System**: Updates dynamically by 20 points whenever a brick is destroyed.
* **Life System**: Players start with 3 lives; the game ends when lives reach zero.
* **Win Condition**: Triggered once all bricks on the screen are destroyed.

## 🎮 Controls
| Key | Action |
| :--- | :--- |
| **Enter** | Start Game (from Welcome Screen)  |
| **Left Arrow** | Move Paddle Left  |
| **Right Arrow** | Move Paddle Right |
| **Esc** | Exit Program  |

## 💻 How to Run
1. Download and install **DOSBox**.
2. Ensure you have the **NASM** assembler and the source code (`project.asm`).
3. Compile the source code in DOSBox:
   ```bash
   nasm project.asm -o project.com
