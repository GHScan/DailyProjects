/*
Navicat SQLite Data Transfer

Source Server         : localhost
Source Server Version : 30706
Source Host           : :0

Target Server Type    : SQLite
Target Server Version : 30706
File Encoding         : 65001

Date: 2014-10-26 23:33:11
*/

PRAGMA foreign_keys = OFF;

-- ----------------------------
-- Table structure for "main"."gangs"
-- ----------------------------
DROP TABLE "main"."gangs";
CREATE TABLE "gangs" (
"id"  INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
"name"  TEXT(16) NOT NULL,
"description"  TEXT(64),
"level"  INTEGER NOT NULL
);

-- ----------------------------
-- Records of gangs
-- ----------------------------

-- ----------------------------
-- Table structure for "main"."items"
-- ----------------------------
DROP TABLE "main"."items";
CREATE TABLE "items" (
"id"  INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
"item_id"  INTEGER NOT NULL,
"player_id"  INTEGER NOT NULL,
"exp"  INTEGER NOT NULL,
CONSTRAINT "foreign_player_id" FOREIGN KEY ("player_id") REFERENCES "players" ("id") ON DELETE CASCADE ON UPDATE RESTRICT
);

-- ----------------------------
-- Records of items
-- ----------------------------

-- ----------------------------
-- Table structure for "main"."players"
-- ----------------------------
DROP TABLE "main"."players";
CREATE TABLE "players" (
"id"  INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
"name"  TEXT(16) NOT NULL,
"gang_id"  INTEGER,
"last_login"  TEXT(20) NOT NULL,
"level"  INTEGER NOT NULL,
"gold"  INTEGER NOT NULL,
"gem"  INTEGER NOT NULL,
CONSTRAINT "foreign_gang_id" FOREIGN KEY ("gang_id") REFERENCES "gangs" ("id") ON DELETE SET NULL ON UPDATE SET NULL
);

-- ----------------------------
-- Records of players
-- ----------------------------

-- ----------------------------
-- Table structure for "main"."sqlite_sequence"
-- ----------------------------
DROP TABLE "main"."sqlite_sequence";
CREATE TABLE sqlite_sequence(name,seq);

-- ----------------------------
-- Records of sqlite_sequence
-- ----------------------------

-- ----------------------------
-- View structure for "main"."gang_members"
-- ----------------------------
DROP VIEW IF EXISTS "main"."gang_members";
CREATE VIEW gang_members
as 
select gangs.*, count(*) as members
from players 
join gangs on players.gang_id = gangs.id 
group by gangs.id
order by members desc;

-- ----------------------------
-- View structure for "main"."gang_players"
-- ----------------------------
DROP VIEW IF EXISTS "main"."gang_players";
CREATE VIEW "gang_players" AS 
select players.name, gangs.name as gang_name, players.level, players.gold, players.gem, count(*) as item_num, sum(items.exp) as item_exps
from items
join players on items.player_id = players.id
join gangs on players.gang_id = gangs.id
group by players.id
order by gangs.level desc, gangs.id, players.level desc;

-- ----------------------------
-- View structure for "main"."players_level"
-- ----------------------------
DROP VIEW IF EXISTS "main"."players_level";
CREATE VIEW "players_level" AS 
select level, avg(gold), avg(gem), count(*)
from players
group by level
order by level desc;

-- ----------------------------
-- Indexes structure for table items
-- ----------------------------
CREATE INDEX "main"."index_player_id"
ON "items" ("player_id" ASC);

-- ----------------------------
-- Indexes structure for table players
-- ----------------------------
CREATE INDEX "main"."index_gang_id"
ON "players" ("gang_id" ASC);
