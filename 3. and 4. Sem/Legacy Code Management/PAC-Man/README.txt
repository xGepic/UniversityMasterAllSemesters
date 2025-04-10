PACMAN TERMINAL GAME (ASCII-based) - Free Pascal Implementation
===============================================================

Author: [Your Name]
Date: [Submission Date]

Description:
-------------
This is a simplified PACMAN game implemented in Free Pascal for the Linux terminal.
The game uses an ASCII grid and keyboard input to simulate the classic arcade gameplay,
focusing on core game logic rather than advanced graphics.

How to Compile:
----------------
1. Open a terminal in Linux.
2. Navigate to the project folder.
3. Compile the code using Free Pascal:
   fpc pacman.pas

How to Run:
------------
After successful compilation, run the executable:
   ./pacman

Controls:
----------
Use the arrow keys to move PACMAN:
←  = Left
→  = Right
↑  = Up
↓  = Down

Game Mechanics:
----------------
- PACMAN collects dots for 10 points each.
- Power Pellets (O) enable PACMAN to eat ghosts for 100 points.
- Ghosts move using basic AI (random movement).
- PACMAN starts with 3 lives.
- Touching a ghost without a power-up causes PACMAN to lose a life.
- Game ends when:
   - All dots are eaten (You win!)
   - All lives are lost (Game Over)

Ghost AI:
----------
Ghosts move randomly each turn, ensuring they do not walk into walls.
While simple, this AI adds unpredictability and danger to gameplay.

External Code Disclaimer:
--------------------------
This project is an original implementation.
No external code libraries or third-party source files were used.
However, standard Free Pascal CRT and keyboard handling routines are used.

Focus of This Project:
-----------------------
The goal is to replicate key gameplay logic (collision detection, scoring,
state changes like power-up modes) rather than build a visually rich version.
Everything is rendered using simple ASCII symbols.

Enjoy the nostalgia!
