INTEGER = {- 2 ^ 31, ..., 2 ^ 31 - 1 }

V = { (x: INTEGER, y: INTEGER, z: INTEGER) }

dot_product = (v_1: V, v_2: V) -> (out: INTEGER) {
    out = v_1.x * v_2.x + v_1.y * v_2.y + v_1.z * v_2.z
}

A = (x: 3, y: -8, z: 0)

B = (x: -4, y: 2, z: A.z)

stdout(dot_product(A, B))
