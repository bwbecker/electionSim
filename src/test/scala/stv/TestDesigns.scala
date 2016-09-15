package stv

/**
  * Created by bwbecker on 2016-09-13.
  */
object TestDesigns {

  val d1 = """
  {
    "design_name": "mmp_small",
    "is_proportional": true,
    "provinces": [
        {
            "prov": "AB",
            "regions": [
                {
                    "region_id": "R1",
                    "top_up_seats": 2,
                    "new_ridings": [
                        {
                            "riding_id": "RidingA",
                            "district_mag": 2,
                            "old_ridings": [
                                "48009, 67, Calgary Nose Hill",
                                "48010, 100, Calgary Rocky Ridge"
                            ]
                        },
                        {
                            "riding_id": "RidingB",
                            "district_mag": 2,
                            "old_ridings": [
                                "48006, 33, Calgary Forest Lawn",
                                "48009, 33, Calgary Nose Hill",
                                "48013, 100, Calgary Skyview"
                            ]
                        }
                    ]
                },
                {
                    "region_id": "R2",
                    "top_up_seats": 0,
                    "new_ridings": [
                        {
                            "riding_id": "RidingC",
                            "district_mag": 1,
                            "old_ridings": [
                                "48005, 100, Calgary Confederation"
                            ]
                        },
                        {
                            "riding_id": "RidingD",
                            "district_mag": 1,
                            "old_ridings": [
                                "48012, 100, Calgary Signal Hill"
                            ]
                        }
                    ]
                }
            ]
        }
    ]
  }
  """
}
