def combine(pre, v1, v2, op):
    if (v1 == '0') or (v2 == '0'):
        return ''
    return pre + v1 + op + v2


def multiplyquat(q1, q2):
    qx = combine('', q1[3], q2[0], '*') + \
        combine(' + ', q1[0], q2[3], '*') + \
        combine(' + ', q1[1], q2[2], '*') + \
        combine(' - ', q1[2], q2[1], '*')

    qy = combine('', q1[3], q2[1], '*') + \
        combine(' - ', q1[0], q2[2], '*') + \
        combine(' + ', q1[1], q2[3], '*') + \
        combine(' + ', q1[2], q2[0], '*')

    qz = combine('', q1[3], q2[2], '*') + \
        combine(' + ', q1[0], q2[1], '*') + \
        combine(' - ', q1[1], q2[0], '*') + \
        combine(' + ', q1[2], q2[3], '*')

    qw = combine('', q1[3], q2[3], '*') + \
        combine(' - ', q1[0], q2[0], '*') + \
        combine(' - ', q1[1], q2[1], '*') + \
        combine(' - ', q1[2], q2[2], '*')

    if not qx:
        qx = '0'
    if not qy:
        qy = '0'
    if not qz:
        qz = '0'
    if not qw:
        qw = '0'
    return [qx, qy, qz, qw]


qx = ['sin(x/2)', '0', '0', 'cos(x/2)']
qy = ['0', 'sin(y/2)', '0', 'cos(y/2)']
qz = ['0', '0', 'sin(z/2)', 'cos(z/2)']


def displayquat(q):
    for line in q:
        print('|' + line)
