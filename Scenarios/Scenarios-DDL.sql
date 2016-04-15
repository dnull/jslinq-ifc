DECLARE @counter smallint;
DECLARE @centerLat float;
DECLARE @centerLon float;
DECLARE @centerOffset float;

-- Customize centerLat and centerLon to your location
-- to get working examples.
--
-- Do not forget to also set this as the base of the grid overlay in
-- AreaAggregation.fs (search for "gridBase")
SET @centerLat = 45.0;
SET @centerLon = 90.0;

CREATE TABLE [dbo].[Item] (
    [Id]   INT         IDENTITY (1, 1) NOT NULL,
    [Name] NCHAR (128) NOT NULL,
    PRIMARY KEY CLUSTERED ([Id] ASC)
);

CREATE TABLE [dbo].[User] (
    [Id]   INT         IDENTITY (1, 1) NOT NULL,
    [Name] NCHAR (128) NOT NULL,
    PRIMARY KEY CLUSTERED ([Id] ASC)
);

CREATE TABLE [dbo].[Event] (
    [Id]        INT           IDENTITY (1, 1) NOT NULL,
    [Timestamp] DATETIME2 (7) NULL,
    [Lat]       FLOAT (53)    NOT NULL,
    [Lon]       FLOAT (53)    NOT NULL,
    [ItemId]    INT           NOT NULL,
    [UserId]    INT           NOT NULL,
    PRIMARY KEY CLUSTERED ([Id] ASC),
    CONSTRAINT [FK_Event_Item] FOREIGN KEY ([ItemId]) REFERENCES [dbo].[Item] ([Id]),
    CONSTRAINT [FK_Event_User] FOREIGN KEY ([UserId]) REFERENCES [dbo].[User] ([Id])
);

CREATE TABLE [dbo].[Points] (
    [Id]        INT         IDENTITY (1, 1) NOT NULL,
    [Latitude]  FLOAT (53)  NOT NULL,
    [Longitude] FLOAT (53)  NOT NULL,
    [Note]      NCHAR (128) NULL,
    [Type]      NCHAR (32)  NULL,
    PRIMARY KEY CLUSTERED ([Id] ASC)
);

CREATE TABLE [dbo].[UserRelation] (
    [User1] INT NOT NULL,
    [User2] INT NOT NULL,
    CONSTRAINT [PK_UserRelation] PRIMARY KEY CLUSTERED ([User1] ASC, [User2] ASC),
    CONSTRAINT [FK_UserRelation_User1] FOREIGN KEY ([User1]) REFERENCES [dbo].[User] ([Id]),
    CONSTRAINT [FK_UserRelation_User2] FOREIGN KEY ([User2]) REFERENCES [dbo].[User] ([Id]),
    CONSTRAINT [CK_UserRelation_Neq] CHECK ([User1]<>[User2])
);

CREATE TABLE [dbo].[AppUser] (
    [Id]        INT         IDENTITY (1, 1) NOT NULL,
	[Name]   NCHAR (32) NULL,
    [PhoneHash]  NCHAR (32)  NOT NULL,
    PRIMARY KEY CLUSTERED ([Id] ASC)
);

INSERT INTO [dbo].[Item] ([Name]) VALUES ('Item A');
INSERT INTO [dbo].[Item] ([Name]) VALUES ('Item B');
INSERT INTO [dbo].[Item] ([Name]) VALUES ('Item C');
INSERT INTO [dbo].[Item] ([Name]) VALUES ('Item D');
INSERT INTO [dbo].[Item] ([Name]) VALUES ('Item E');
INSERT INTO [dbo].[Item] ([Name]) VALUES ('Item F');
INSERT INTO [dbo].[Item] ([Name]) VALUES ('Item G');

INSERT INTO [dbo].[User] ([Name]) VALUES ('User A');
INSERT INTO [dbo].[User] ([Name]) VALUES ('User B');
INSERT INTO [dbo].[User] ([Name]) VALUES ('User C');

INSERT INTO [dbo].[UserRelation] ([User1], [User2]) VALUES (2, 1)

-- Insert some numbers of your contacts here to have some matches
INSERT INTO [dbo].[AppUser] ([Name], [PhoneHash]) VALUES ('AAA', '870a4998728e86771ae52cf3ea0f773b');

-- Generate random events
SET @counter = 1;
SET @centerOffset = 0.05

WHILE @counter < 1000
	BEGIN
		INSERT INTO dbo.[Event] (Lat, Lon, ItemId, UserId)
		VALUES (
			@centerLat + (RAND()*2*@centerOffset)-@centerOffset,
			@centerLon + (RAND()*2*@centerOffset)-@centerOffset,
			CEILING(RAND()*(SELECT MAX(Id) FROM dbo.[Item])),
			CEILING(RAND()*(SELECT MAX(Id) FROM dbo.[User]))
			)
		SET @counter = @counter + 1 
	END;

-- Generate random Points of Interest
SET @counter = 1;
SET @centerOffset = 0.2

WHILE @counter < 100
	BEGIN
		INSERT INTO dbo.[Points] (Latitude, Longitude, Note)
		VALUES (
			@centerLat + (RAND()*2*@centerOffset)-@centerOffset,
			@centerLon + (RAND()*2*@centerOffset)-@centerOffset,
			CONCAT('Point ', @counter)
			)
		SET @counter = @counter + 1 
	END;
GO