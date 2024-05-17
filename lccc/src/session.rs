use core::cell::RefCell;
use std::path::PathBuf;

use xlang::abi::io::{Read, Write};
use xlang::abi::prelude::v1::*;
use xlang::abi::string::StringView;
use xlang::plugin::{XLangCodegen, XLangFrontend, XLangPlugin};
use xlang::targets::properties::{MachineProperties, TargetProperties};
use xlang_host::dso::Handle;

use crate::Mode;

use super::{DebugLevel, OptimizeLevel};

pub struct MsvcSysroot {
    // TODO: Someone with windows actually fill in what we need for this
}

pub struct DriverSession<'a> {
    pub targ_name: StringView<'a>,
    pub targ_properties: &'a TargetProperties<'a>,
    pub mach: &'a MachineProperties<'a>,
    pub features: Vec<StringView<'a>>,
    pub is_crosscompiling: bool,
    pub opt_goal: OptimizeLevel,
    pub debug_info: DebugLevel,
    pub msvc: Option<MsvcSysroot>,
    plugin_cache: RefCell<Vec<Handle>>,
    frontend_cache: RefCell<Vec<Handle>>,
    backend_cache: RefCell<Option<Handle>>,
}

impl<'a> DriverSession<'a> {
    pub fn from_target(
        targ_name: StringView<'a>,
        targ_properties: &'a TargetProperties<'a>,
        is_crosscompiling: bool,
    ) -> Self {
        Self {
            targ_name,
            targ_properties,
            is_crosscompiling,
            mach: targ_properties.arch.default_machine,
            features: Vec::new(),
            opt_goal: OptimizeLevel::Integer(0),
            debug_info: DebugLevel::None,
            msvc: None,
            plugin_cache: RefCell::new(Vec::new()),
            frontend_cache: RefCell::new(Vec::new()),
            backend_cache: RefCell::new(None),
        }
    }
    pub fn find_msvc(&mut self) {
        #[cfg(target_os_family = "windows")]
        {
            if !self.is_crosscompiling {
                // Find msvc install
                self.msvc = Some(MsvcSysroot {})
            }
        }
    }
}

pub struct CompileSession<'a> {
    driver: &'a DriverSession<'a>,
    frontend: DynBox<dyn XLangFrontend>,
    backend: Option<DynBox<dyn XLangCodegen>>,
    middle_plugins: Vec<DynBox<dyn XLangPlugin>>,
    file: PathBuf,
    output_file: Box<dyn Write + 'a>,
}

impl<'a> CompileSession<'a> {}
