# [Arcade: Universal Cosmic Guerilla](https://www.arcade-museum.com/game_detail.php?game_id=7402) for [MiSTer](https://github.com/MiSTer-devel/Main_MiSTer/wiki)

By [Mike Coates](https://github.com/macrofpga)  
Current Version - 1.00 - 04/04/2023

## Description

This is a recreation of the [Universal Cosmic Guerilla](https://www.arcade-museum.com/manuf_detail.php?manuf_id=1703&orig_game_id=7402) game.

The game timing should be very close to the original, but the code is not necessarily identical to the real thing, but achieves a similar end result.

## Controls

Left, Right, Fire 1

## Known differences/problems

This core uses a TMS9900 CPU where the original uses a TMS9980 (8 bit version of same CPU, just does 2 memory reads/writes instead of 1 16 bit read/write). Game speed is controlled using vertical counter, so should still be the same as the original.

Sound effects are all implemented using samples.

Watchdog is not implemented (since it uses a weird watchdog setup, and all my efforts to recreate one with the same timing results in it rebooting constantly)

It does seem possible to crash the game, especially by adding lots of coins quickly. It also does this in Mame. I guess in the real world you would not be able to put coins in quick enough to make it happen.

## ROM Files Instructions

**ROMs are not included!** In order to use this arcade core, you will need to provide the correct ROM file yourself.

To simplify the process `.mra` files are provided in the releases folder, that specify the required ROMs with their checksums. The ROMs `.zip` filename refers to the corresponding file from the MAME project.

Please refer to https://github.com/MiSTer-devel/Main_MiSTer/wiki/Arcade-Roms for information on how to setup and use the environment.

Quick reference for folders and file placement:

```
/_Arcade/<game name>.mra
/_Arcade/cores/<game rbf>.rbf
/games/mame/<mame rom>.zip
/games/hbmame/<hbmame rom>.zip
```
