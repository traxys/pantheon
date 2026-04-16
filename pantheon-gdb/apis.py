from typing import Generator
import gdb
import gdb.printing
import re

GdbChild = tuple[str, gdb.Value | str]


def unwrap_unique_or_non_null(unique_or_nonnull):
    ptr = unique_or_nonnull["pointer"]
    return ptr if ptr.type.code == gdb.TYPE_CODE_PTR else ptr[ptr.type.fields()[0]]


def _enumerate_array_elements(
    element_ptrs,
) -> Generator[GdbChild, None, None]:
    for i, element_ptr in enumerate(element_ptrs):
        key = "[{}]".format(i)
        element = element_ptr.dereference()

        try:
            # rust-lang/rust#64343: passing deref expr to `str` allows
            # catching exception on garbage pointer
            str(element)
        except RuntimeError:
            yield key, "inaccessible"

            break

        yield key, element


class ApisVecProvider(gdb.ValuePrinter):
    "Print a apis::collections::Vec"

    NAME = "ApisVec"
    REGEX = re.compile(r"^(apis::([a-z_]+::)+)Vec<.+>$")

    def __init__(self, val: gdb.Value):
        self._val = val
        self._length = int(val["len"])
        self._data_ptr = unwrap_unique_or_non_null(val["data"])
        ptr_ty = gdb.Type.pointer(val.type.template_argument(0))
        self._data_ptr = self._data_ptr.reinterpret_cast(ptr_ty)

    def to_string(self):
        return "apis::Vec(size={})".format(self._length)

    def children(self) -> Generator[GdbChild, None, None]:
        return _enumerate_array_elements(
            self._data_ptr + index for index in range(self._length)
        )

    @staticmethod
    def display_hint():
        return "array"

    def num_children(self):
        return self._length


class ApisSubPrinter(gdb.printing.SubPrettyPrinter):
    def __init__(self, name, provider):
        super(ApisSubPrinter, self).__init__(name)
        self.provider = provider

    def match(self, ty) -> bool:
        return self.provider.REGEX.match(ty)

    def __call__(self, val):
        if self.enabled:
            return self.provider(val)
        else:
            return None


class ApisProvider(gdb.printing.PrettyPrinter):
    def __init__(self, name):
        super(ApisProvider, self).__init__(name, [])

        self._providers: list[ApisSubPrinter] = []

        assert self.subprinters is not None
        for provider in [ApisVecProvider]:
            printer = ApisSubPrinter(provider.NAME, provider)
            self._providers.append(printer)
            self.subprinters.append(printer)

    def __call__(self, val):
        if val.type.code == gdb.TYPE_CODE_STRUCT:
            for provider in self._providers:
                if provider.match(val.type.tag):
                    return provider(val)

        return None
