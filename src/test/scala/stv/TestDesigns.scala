package stv

import stv.Party._
import stv.io.Input.RawCandidate

/**
  * Created by bwbecker on 2016-09-13.
  */
object TestDesigns {

  val c1 = Vector[RawCandidate](
    RawCandidate(1, "C1-C", Con, 30),
    RawCandidate(1, "C1-L", Lib, 20),
    RawCandidate(1, "C1-N", NDP, 10),

    RawCandidate(2, "C2-C", Con, 100),
    RawCandidate(2, "C2-L", Lib, 200),
    RawCandidate(2, "C2-N", NDP, 102),

    RawCandidate(3, "C3-C", Con, 50),
    RawCandidate(3, "C3-L", Lib, 60),
    RawCandidate(3, "C3-N", NDP, 10),

    RawCandidate(4, "C4-C", Con, 20),
    RawCandidate(4, "C4-L", Lib, 34),
    RawCandidate(4, "C4-N", NDP, 22),

    RawCandidate(5, "C5-C", Con, 30),
    RawCandidate(5, "C5-L", Lib, 24),
    RawCandidate(5, "C5-N", NDP, 10),

    RawCandidate(6, "C6-C", Con, 70),
    RawCandidate(6, "C6-L", Lib, 20),
    RawCandidate(6, "C6-N", NDP, 30)

  )

  val r1 = Vector[RawFptpRiding](
    RawFptpRiding(1, ProvName.AB, "Riding_1", 100, 1000, 1),
    RawFptpRiding(2, ProvName.AB, "Riding_2", 200, 2000, 1),
    RawFptpRiding(3, ProvName.AB, "Riding_3", 300, 3000, 1),
    RawFptpRiding(4, ProvName.AB, "Riding_4", 400, 4000, 1),
    RawFptpRiding(5, ProvName.AB, "Riding_5", 500, 5000, 1),
    RawFptpRiding(6, ProvName.AB, "Riding_6", 600, 6000, 1)
  )

  // ERRE compliant; test riding centric stuff
  val drc = """
  {
    "design_name": "erre_ru_multiples_10pct",
    "description": [["Test riding centric stuff"]],
    "election_strategies": ["RcRUPR"],
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
                            "district_mag": 1,
                            "old_ridings": [
                                "1, 100, Riding_1",
                                "2, 100, Riding_2"
                            ]
                        },
                        {
                            "riding_id": "RidingB",
                            "district_mag": 1,
                            "old_ridings": [
                                "3, 100, Riding_3",
                                "4, 100, Riding_4"
                            ]
                        }
                    ]
                },
                {
                    "region_id": "R2",
                    "top_up_seats": 1,
                    "new_ridings": [
                        {
                            "riding_id": "RidingC",
                            "district_mag": 1,
                            "old_ridings": [
                                "5, 100, Riding_5",
                                "6, 100, Riding_6"
                            ]
                        }
                    ]
                }
            ]
        }
    ]
  }
  """

  val d1 = """
  {
    "design_name": "mmp_small",
    "description": [["d1"]],
    "election_strategies": ["STV"],
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
                                "1, 60, Riding_1",
                                "2, 100, Riding_2",
                                "4, 50, Riding_4"
                            ]
                        },
                        {
                            "riding_id": "RidingB",
                            "district_mag": 2,
                            "old_ridings": [
                                "1, 40, Riding_1",
                                "3, 100, Riding_3",
                                "4, 50, Riding_4"
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
                                "5, 100, Riding_5"
                            ]
                        },
                        {
                            "riding_id": "RidingD",
                            "district_mag": 1,
                            "old_ridings": [
                                "6, 100, Riding_6"
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
