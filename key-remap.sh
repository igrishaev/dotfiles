
# https://superuser.com/questions/37042/remapping-of-keys-in-mac-os-x
# https://developer.apple.com/library/archive/technotes/tn2450/_index.html


hidutil property --set '{"UserKeyMapping":[{"HIDKeyboardModifierMappingSrc":0x700000039,"HIDKeyboardModifierMappingDst":0x7000000E1},{"HIDKeyboardModifierMappingSrc":0x7000000E7,"HIDKeyboardModifierMappingDst":0x7000000E6},{"HIDKeyboardModifierMappingSrc":0x7000000E6,"HIDKeyboardModifierMappingDst":0x7000000E4}]}'

hidutil property --get "UserKeyMapping"

# caps          left shift
# 0x39          0xE1

# right cmd     left option
# 0xE7          0xE6

# right option  right control
# 0xE6          0xE4


# UserKeyMapping:(
#         {
#         HIDKeyboardModifierMappingDst = 30064771297;
#         HIDKeyboardModifierMappingSrc = 30064771129;
#     },
#         {
#         HIDKeyboardModifierMappingDst = 30064771302;
#         HIDKeyboardModifierMappingSrc = 30064771303;
#     },
#         {
#         HIDKeyboardModifierMappingDst = 30064771300;
#         HIDKeyboardModifierMappingSrc = 30064771302;
#     }
# )
