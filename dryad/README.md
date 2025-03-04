# Flight altitudes of American Woodcock

### Description of the data and file structure
We collected GPS locations and GPS-derived altitude data using PinPoint GPS transmitters attached to American Woodcock (*Scolopax minor*) throughout the eastern portion of their range from 2020 to 2024. This dataset includes both ground locations (i.e., locations recorded when the bird was not undergoing migratory flight) and flight locations; the procedure for differentiating between these locations is outlined in the accompanying paper in *Ornithological Applications*. 

#### Files and variables

##### File: amwo_flight_altitudes.csv
**Description**: This csv file contains GPS locations and altitude data collected from American Woodcock, as well as additional covariates recorded on capture or imported from other sources. The data has 16020 rows of 13 variables. 

1. ID: Unique ID for each woodcock.

2. time: Time at which the GPS location/altitude were recorded in Coordinated Universal Time.

3. lon: Longitude of the GPS location.

4. lat: Latitude of the GPS location.

5. sex: Sex of the woodcock. "m" is male, "f" is female, "u" is unknown.

6. age: Age of the woodcock when it was originally marked, following the Humphrey-Parkes system for categorizing bird ages. Age classes are "After Second Year", "Second Year", "After Hatch Year", "Hatch Year", and "Unknown".

7. height_above_wgs84: GPS-derived altitude of the location, recorded in meters above the WGS84 ellipsoid.

8. height_above_terrain: Processed GPS altitude, indicating the altitude (meters) above an orthometric elevation (i.e., terrain) layer (ESRI 2024b). This can be interpreted as altitude above ground level.

9. migratory_state: Movement state classifications from Berigan (2024), showing whether a given GPS location was recorded during a migration or a long-distance movement. Options include:
    - "Point state: Stationary": Not recorded during a migration or long-distance movement.
    - "Point state: Migratory (spring)": Recorded during spring migration.
    - "Point state: Migratory (fall)": Recorded during fall migration.
    - "Point state: Migratory (summer)": Recorded during summer migration, described by Berigan (2024) as southerly migratory movements which occurred prior to August 1st.
    - "Point state: Foray loop": Foray loops are defined as circular or out-and-back movements with steps ≥16.1 km that result in <16.1 km of net displacement between the first and last point. Foray loops begin and end in a non-migratory state.

10. moving: Binary variable indicating whether a given GPS location is ≥6.67 km from preceding and following points. Points with a FALSE designation are almost always ground locations.

11. day_night: Indicates whether a location was recorded during the day (after sunrise and before sunset) or during the night. Determined using the *suncalc* package in R (Version 0.5.1).

12. known_ground_location: Binary variable indicating whether a location was treated as a known ground location (i.e., recorded during the day).

13. possible_flight_location: Binary variable indicating whether a location was treated as a potential flight location (i.e., nocturnal, migratory, ≥6.67 km from preceding and following points).

### Code/software

Any spreadsheet program (e.g., Microsoft Excel) can be used to access these data. The code required to run this analysis in its entirety is stored at https://github.com/EWMRC/flight-altitude/.

### Access information

There are no other publicly accessible locations of the data at the time of publication.
