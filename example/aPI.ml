type failure = string list [@@deriving rpc]

let response_of_failure code params =
  Rpc.failure (rpc_of_failure (code :: params))

let response_of_fault code = Rpc.failure (rpc_of_failure ["Fault"; code])

type cluster_host_operation = [`enable | `disable | `destroy]
[@@deriving rpcty]

type cluster_operation = [`add | `remove | `enable | `disable | `destroy]
[@@deriving rpcty]

type vusb_operations = [`attach | `plug | `unplug] [@@deriving rpcty]

type sdn_controller_protocol = [`ssl | `pssl] [@@deriving rpcty]

type pvs_proxy_status =
  [ `stopped
  | `initialised
  | `caching
  | `incompatible_write_cache_mode
  | `incompatible_protocol_version ]
[@@deriving rpcty]

type vgpu_type_implementation = [`passthrough | `nvidia | `gvt_g | `mxgpu]
[@@deriving rpcty]

type allocation_algorithm = [`breadth_first | `depth_first] [@@deriving rpcty]

type pgpu_dom0_access =
  [`enabled | `disable_on_reboot | `disabled | `enable_on_reboot]
[@@deriving rpcty]

type sriov_configuration_mode = [`sysfs | `modprobe | `unknown]
[@@deriving rpcty]

type cls = [`VM | `Host | `SR | `Pool | `VMPP | `VMSS | `PVS_proxy | `VDI]
[@@deriving rpcty]

type console_protocol = [`vt100 | `rfb | `rdp] [@@deriving rpcty]

type vbd_mode = [`RO | `RW] [@@deriving rpcty]

type vbd_type = [`CD | `Disk | `Floppy] [@@deriving rpcty]

type vbd_operations =
  [ `attach
  | `eject
  | `insert
  | `plug
  | `unplug
  | `unplug_force
  | `pause
  | `unpause ]
[@@deriving rpcty]

type on_boot = [`reset | `persist] [@@deriving rpcty]

type vdi_type =
  [ `system
  | `user
  | `ephemeral
  | `suspend
  | `crashdump
  | `ha_statefile
  | `metadata
  | `redo_log
  | `rrd
  | `pvs_cache
  | `cbt_metadata ]
[@@deriving rpcty]

type vdi_operations =
  [ `clone
  | `copy
  | `resize
  | `resize_online
  | `snapshot
  | `mirror
  | `destroy
  | `forget
  | `update
  | `force_unlock
  | `generate_config
  | `enable_cbt
  | `disable_cbt
  | `data_destroy
  | `list_changed_blocks
  | `set_on_boot
  | `blocked ]
[@@deriving rpcty]

type storage_operations =
  [ `scan
  | `destroy
  | `forget
  | `plug
  | `unplug
  | `update
  | `vdi_create
  | `vdi_introduce
  | `vdi_destroy
  | `vdi_resize
  | `vdi_clone
  | `vdi_snapshot
  | `vdi_mirror
  | `vdi_enable_cbt
  | `vdi_disable_cbt
  | `vdi_data_destroy
  | `vdi_list_changed_blocks
  | `vdi_set_on_boot
  | `pbd_create
  | `pbd_destroy ]
[@@deriving rpcty]

type bond_mode = [`balanceslb | `activebackup | `lacp] [@@deriving rpcty]

type primary_address_type = [`IPv4 | `IPv6] [@@deriving rpcty]

type ipv6_configuration_mode = [`None | `DHCP | `Static | `Autoconf]
[@@deriving rpcty]

type ip_configuration_mode = [`None | `DHCP | `Static] [@@deriving rpcty]

type pif_igmp_status = [`enabled | `disabled | `unknown] [@@deriving rpcty]

type vif_ipv6_configuration_mode = [`None | `Static] [@@deriving rpcty]

type vif_ipv4_configuration_mode = [`None | `Static] [@@deriving rpcty]

type vif_locking_mode = [`network_default | `locked | `unlocked | `disabled]
[@@deriving rpcty]

type vif_operations = [`attach | `plug | `unplug] [@@deriving rpcty]

type network_purpose = [`nbd | `insecure_nbd] [@@deriving rpcty]

type network_default_locking_mode = [`unlocked | `disabled] [@@deriving rpcty]

type network_operations = [`attaching] [@@deriving rpcty]

type host_display =
  [`enabled | `disable_on_reboot | `disabled | `enable_on_reboot]
[@@deriving rpcty]

type host_allowed_operations =
  [ `provision
  | `evacuate
  | `shutdown
  | `reboot
  | `power_on
  | `vm_start
  | `vm_resume
  | `vm_migrate ]
[@@deriving rpcty]

type vm_appliance_operation =
  [`start | `clean_shutdown | `hard_shutdown | `shutdown]
[@@deriving rpcty]

type vmss_type = [`snapshot | `checkpoint | `snapshot_with_quiesce]
[@@deriving rpcty]

type vmss_frequency = [`hourly | `daily | `weekly] [@@deriving rpcty]

type vmpp_archive_target_type = [`none | `cifs | `nfs] [@@deriving rpcty]

type vmpp_archive_frequency = [`never | `always_after_backup | `daily | `weekly]
[@@deriving rpcty]

type vmpp_backup_frequency = [`hourly | `daily | `weekly] [@@deriving rpcty]

type vmpp_backup_type = [`snapshot | `checkpoint] [@@deriving rpcty]

type tristate_type = [`yes | `no | `unspecified] [@@deriving rpcty]

type domain_type = [`hvm | `pv | `pv_in_pvh | `unspecified] [@@deriving rpcty]

type on_crash_behaviour =
  [ `destroy
  | `coredump_and_destroy
  | `restart
  | `coredump_and_restart
  | `preserve
  | `rename_restart ]
[@@deriving rpcty]

type vm_operations =
  [ `snapshot
  | `clone
  | `copy
  | `create_template
  | `revert
  | `checkpoint
  | `snapshot_with_quiesce
  | `provision
  | `start
  | `start_on
  | `pause
  | `unpause
  | `clean_shutdown
  | `clean_reboot
  | `hard_shutdown
  | `power_state_reset
  | `hard_reboot
  | `suspend
  | `csvm
  | `resume
  | `resume_on
  | `pool_migrate
  | `migrate_send
  | `get_boot_record
  | `send_sysrq
  | `send_trigger
  | `query_services
  | `shutdown
  | `call_plugin
  | `changing_memory_live
  | `awaiting_memory_live
  | `changing_dynamic_range
  | `changing_static_range
  | `changing_memory_limits
  | `changing_shadow_memory
  | `changing_shadow_memory_live
  | `changing_VCPUs
  | `changing_VCPUs_live
  | `changing_NVRAM
  | `assert_operation_valid
  | `data_source_op
  | `update_allowed_operations
  | `make_into_template
  | `import
  | `export
  | `metadata_export
  | `reverting
  | `destroy ]
[@@deriving rpcty]

type on_normal_exit = [`destroy | `restart] [@@deriving rpcty]

type vm_power_state = [`Halted | `Paused | `Running | `Suspended]
[@@deriving rpcty]

type update_after_apply_guidance =
  [`restartHVM | `restartPV | `restartHost | `restartXAPI]
[@@deriving rpcty]

type after_apply_guidance =
  [`restartHVM | `restartPV | `restartHost | `restartXAPI]
[@@deriving rpcty]

type pool_allowed_operations = [`ha_enable | `ha_disable | `cluster_create]
[@@deriving rpcty]

type task_status_type =
  [`pending | `success | `failure | `cancelling | `cancelled]
[@@deriving rpcty]

type task_allowed_operations = [`cancel | `destroy] [@@deriving rpcty]

type hello_return = [`ok | `unknown_host | `cannot_talk_back]
[@@deriving rpcty]

type livepatch_status =
  [`ok_livepatch_complete | `ok_livepatch_incomplete | `ok]
[@@deriving rpcty]

type sr_health = [`healthy | `recovering] [@@deriving rpcty]

type event_operation = [`add | `del | `_mod] [@@deriving rpcty]

type cluster_host_ref = cluster_host_t Rpc.Types.ref

and pIF_ref = pIF_t Rpc.Types.ref

and host_ref = host_t Rpc.Types.ref

and cluster_ref = cluster_t Rpc.Types.ref

and network_ref = network_t Rpc.Types.ref

and vUSB_ref = vUSB_t Rpc.Types.ref

and uSB_group_ref = uSB_group_t Rpc.Types.ref

and vM_ref = vM_t Rpc.Types.ref

and pUSB_ref = pUSB_t Rpc.Types.ref

and sDN_controller_ref = sDN_controller_t Rpc.Types.ref

and feature_ref = feature_t Rpc.Types.ref

and vDI_ref = vDI_t Rpc.Types.ref

and pVS_cache_storage_ref = pVS_cache_storage_t Rpc.Types.ref

and pVS_site_ref = pVS_site_t Rpc.Types.ref

and sR_ref = sR_t Rpc.Types.ref

and pVS_proxy_ref = pVS_proxy_t Rpc.Types.ref

and vIF_ref = vIF_t Rpc.Types.ref

and pVS_server_ref = pVS_server_t Rpc.Types.ref

and vGPU_type_ref = vGPU_type_t Rpc.Types.ref

and gPU_group_ref = gPU_group_t Rpc.Types.ref

and pGPU_ref = pGPU_t Rpc.Types.ref

and vGPU_ref = vGPU_t Rpc.Types.ref

and pCI_ref = pCI_t Rpc.Types.ref

and network_sriov_ref = network_sriov_t Rpc.Types.ref

and tunnel_ref = tunnel_t Rpc.Types.ref

and secret_ref = secret_t Rpc.Types.ref

and message_ref = message_t Rpc.Types.ref

and blob_ref = blob_t Rpc.Types.ref

and user_ref = user_t Rpc.Types.ref

and console_ref = console_t Rpc.Types.ref

and vTPM_ref = vTPM_t Rpc.Types.ref

and crashdump_ref = crashdump_t Rpc.Types.ref

and pBD_ref = pBD_t Rpc.Types.ref

and vBD_metrics_ref = vBD_metrics_t Rpc.Types.ref

and vBD_ref = vBD_t Rpc.Types.ref

and pool_ref = pool_t Rpc.Types.ref

and lVHD_ref = lVHD_t Rpc.Types.ref

and dR_task_ref = dR_task_t Rpc.Types.ref

and sM_ref = sM_t Rpc.Types.ref

and vLAN_ref = vLAN_t Rpc.Types.ref

and bond_ref = bond_t Rpc.Types.ref

and pIF_metrics_ref = pIF_metrics_t Rpc.Types.ref

and vIF_metrics_ref = vIF_metrics_t Rpc.Types.ref

and host_cpu_ref = host_cpu_t Rpc.Types.ref

and host_metrics_ref = host_metrics_t Rpc.Types.ref

and host_patch_ref = host_patch_t Rpc.Types.ref

and pool_patch_ref = pool_patch_t Rpc.Types.ref

and host_crashdump_ref = host_crashdump_t Rpc.Types.ref

and pool_update_ref = pool_update_t Rpc.Types.ref

and session_ref = session_t Rpc.Types.ref

and vM_appliance_ref = vM_appliance_t Rpc.Types.ref

and vMSS_ref = vMSS_t Rpc.Types.ref

and vMPP_ref = vMPP_t Rpc.Types.ref

and vM_guest_metrics_ref = vM_guest_metrics_t Rpc.Types.ref

and vM_metrics_ref = vM_metrics_t Rpc.Types.ref

and task_ref = task_t Rpc.Types.ref

and role_ref = role_t Rpc.Types.ref

and subject_ref = subject_t Rpc.Types.ref

and vdi_nbd_server_info_ref = vdi_nbd_server_info_t Rpc.Types.ref

and data_source_ref = data_source_t Rpc.Types.ref

and probe_result_ref = probe_result_t Rpc.Types.ref

and sr_stat_ref = sr_stat_t Rpc.Types.ref

and event_ref = event_t Rpc.Types.ref

and sr_stat_t =
  { sr_stat_uuid: string option
  ; sr_stat_name_label: string
  ; sr_stat_name_description: string
  ; sr_stat_free_space: int64
  ; sr_stat_total_space: int64
  ; sr_stat_clustered: bool
  ; sr_stat_health: [`healthy | `recovering] }

and cluster_host_t =
  { cluster_host_uuid: string
  ; cluster_host_cluster: cluster_t Rpc.Types.ref
  ; cluster_host_host: host_t Rpc.Types.ref
  ; cluster_host_enabled: bool
  ; cluster_host_PIF: pIF_t Rpc.Types.ref
  ; cluster_host_joined: bool
  ; cluster_host_allowed_operations: [`enable | `disable | `destroy] list
  ; cluster_host_current_operations: (string * cluster_host_operation) list
  ; cluster_host_other_config: (string * string) list }

and cluster_t =
  { cluster_uuid: string
  ; cluster_cluster_hosts: cluster_host_t Rpc.Types.ref list
  ; cluster_pending_forget: string list
  ; cluster_cluster_token: string
  ; cluster_cluster_stack: string
  ; cluster_allowed_operations:
      [`add | `remove | `enable | `disable | `destroy] list
  ; cluster_current_operations: (string * cluster_operation) list
  ; cluster_pool_auto_join: bool
  ; cluster_token_timeout: float
  ; cluster_token_timeout_coefficient: float
  ; cluster_cluster_config: (string * string) list
  ; cluster_other_config: (string * string) list }

and vUSB_t =
  { vUSB_uuid: string
  ; vUSB_allowed_operations: [`attach | `plug | `unplug] list
  ; vUSB_current_operations: (string * vusb_operations) list
  ; vUSB_VM: vM_t Rpc.Types.ref
  ; vUSB_USB_group: uSB_group_t Rpc.Types.ref
  ; vUSB_other_config: (string * string) list
  ; vUSB_currently_attached: bool }

and uSB_group_t =
  { uSB_group_uuid: string
  ; uSB_group_name_label: string
  ; uSB_group_name_description: string
  ; uSB_group_PUSBs: pUSB_t Rpc.Types.ref list
  ; uSB_group_VUSBs: vUSB_t Rpc.Types.ref list
  ; uSB_group_other_config: (string * string) list }

and pUSB_t =
  { pUSB_uuid: string
  ; pUSB_USB_group: uSB_group_t Rpc.Types.ref
  ; pUSB_host: host_t Rpc.Types.ref
  ; pUSB_path: string
  ; pUSB_vendor_id: string
  ; pUSB_vendor_desc: string
  ; pUSB_product_id: string
  ; pUSB_product_desc: string
  ; pUSB_serial: string
  ; pUSB_version: string
  ; pUSB_description: string
  ; pUSB_passthrough_enabled: bool
  ; pUSB_other_config: (string * string) list }

and sDN_controller_t =
  { sDN_controller_uuid: string
  ; sDN_controller_protocol: [`ssl | `pssl]
  ; sDN_controller_address: string
  ; sDN_controller_port: int64 }

and feature_t =
  { feature_uuid: string
  ; feature_name_label: string
  ; feature_name_description: string
  ; feature_enabled: bool
  ; feature_experimental: bool
  ; feature_version: string
  ; feature_host: host_t Rpc.Types.ref }

and pVS_cache_storage_t =
  { pVS_cache_storage_uuid: string
  ; pVS_cache_storage_host: host_t Rpc.Types.ref
  ; pVS_cache_storage_SR: sR_t Rpc.Types.ref
  ; pVS_cache_storage_site: pVS_site_t Rpc.Types.ref
  ; pVS_cache_storage_size: int64
  ; pVS_cache_storage_VDI: vDI_t Rpc.Types.ref }

and pVS_proxy_t =
  { pVS_proxy_uuid: string
  ; pVS_proxy_site: pVS_site_t Rpc.Types.ref
  ; pVS_proxy_VIF: vIF_t Rpc.Types.ref
  ; pVS_proxy_currently_attached: bool
  ; pVS_proxy_status:
      [ `stopped
      | `initialised
      | `caching
      | `incompatible_write_cache_mode
      | `incompatible_protocol_version ] }

and pVS_server_t =
  { pVS_server_uuid: string
  ; pVS_server_addresses: string list
  ; pVS_server_first_port: int64
  ; pVS_server_last_port: int64
  ; pVS_server_site: pVS_site_t Rpc.Types.ref }

and pVS_site_t =
  { pVS_site_uuid: string
  ; pVS_site_name_label: string
  ; pVS_site_name_description: string
  ; pVS_site_PVS_uuid: string
  ; pVS_site_cache_storage: pVS_cache_storage_t Rpc.Types.ref list
  ; pVS_site_servers: pVS_server_t Rpc.Types.ref list
  ; pVS_site_proxies: pVS_proxy_t Rpc.Types.ref list }

and vGPU_type_t =
  { vGPU_type_uuid: string
  ; vGPU_type_vendor_name: string
  ; vGPU_type_model_name: string
  ; vGPU_type_framebuffer_size: int64
  ; vGPU_type_max_heads: int64
  ; vGPU_type_max_resolution_x: int64
  ; vGPU_type_max_resolution_y: int64
  ; vGPU_type_size: int64
  ; vGPU_type_supported_on_PGPUs: pGPU_t Rpc.Types.ref list
  ; vGPU_type_enabled_on_PGPUs: pGPU_t Rpc.Types.ref list
  ; vGPU_type_VGPUs: vGPU_t Rpc.Types.ref list
  ; vGPU_type_internal_config: (string * string) list
  ; vGPU_type_supported_on_GPU_groups: gPU_group_t Rpc.Types.ref list
  ; vGPU_type_enabled_on_GPU_groups: gPU_group_t Rpc.Types.ref list
  ; vGPU_type_implementation: [`passthrough | `nvidia | `gvt_g | `mxgpu]
  ; vGPU_type_identifier: string
  ; vGPU_type_experimental: bool }

and vGPU_t =
  { vGPU_uuid: string
  ; vGPU_VM: vM_t Rpc.Types.ref
  ; vGPU_GPU_group: gPU_group_t Rpc.Types.ref
  ; vGPU_device: string
  ; vGPU_currently_attached: bool
  ; vGPU_other_config: (string * string) list
  ; vGPU_type: vGPU_type_t Rpc.Types.ref
  ; vGPU_resident_on: pGPU_t Rpc.Types.ref
  ; vGPU_scheduled_to_be_resident_on: pGPU_t Rpc.Types.ref
  ; vGPU_compatibility_metadata: (string * string) list }

and gPU_group_t =
  { gPU_group_uuid: string
  ; gPU_group_name_label: string
  ; gPU_group_name_description: string
  ; gPU_group_PGPUs: pGPU_t Rpc.Types.ref list
  ; gPU_group_VGPUs: vGPU_t Rpc.Types.ref list
  ; gPU_group_GPU_types: string list
  ; gPU_group_other_config: (string * string) list
  ; gPU_group_allocation_algorithm: [`breadth_first | `depth_first]
  ; gPU_group_supported_VGPU_types: vGPU_type_t Rpc.Types.ref list
  ; gPU_group_enabled_VGPU_types: vGPU_type_t Rpc.Types.ref list }

and pGPU_t =
  { pGPU_uuid: string
  ; pGPU_PCI: pCI_t Rpc.Types.ref
  ; pGPU_GPU_group: gPU_group_t Rpc.Types.ref
  ; pGPU_host: host_t Rpc.Types.ref
  ; pGPU_other_config: (string * string) list
  ; pGPU_supported_VGPU_types: vGPU_type_t Rpc.Types.ref list
  ; pGPU_enabled_VGPU_types: vGPU_type_t Rpc.Types.ref list
  ; pGPU_resident_VGPUs: vGPU_t Rpc.Types.ref list
  ; pGPU_size: int64
  ; pGPU_supported_VGPU_max_capacities: (vGPU_type_ref * int64) list
  ; pGPU_dom0_access:
      [`enabled | `disable_on_reboot | `disabled | `enable_on_reboot]
  ; pGPU_is_system_display_device: bool
  ; pGPU_compatibility_metadata: (string * string) list }

and pCI_t =
  { pCI_uuid: string
  ; pCI_class_id: string
  ; pCI_class_name: string
  ; pCI_vendor_id: string
  ; pCI_vendor_name: string
  ; pCI_device_id: string
  ; pCI_device_name: string
  ; pCI_host: host_t Rpc.Types.ref
  ; pCI_pci_id: string
  ; pCI_functions: int64
  ; pCI_virtual_functions: pCI_t Rpc.Types.ref list
  ; pCI_physical_function: pCI_t Rpc.Types.ref
  ; pCI_attached_VMs: vM_t Rpc.Types.ref list
  ; pCI_dependencies: pCI_t Rpc.Types.ref list
  ; pCI_other_config: (string * string) list
  ; pCI_subsystem_vendor_id: string
  ; pCI_subsystem_vendor_name: string
  ; pCI_subsystem_device_id: string
  ; pCI_subsystem_device_name: string
  ; pCI_scheduled_to_be_attached_to: vM_t Rpc.Types.ref
  ; pCI_driver_name: string }

and network_sriov_t =
  { network_sriov_uuid: string
  ; network_sriov_physical_PIF: pIF_t Rpc.Types.ref
  ; network_sriov_logical_PIF: pIF_t Rpc.Types.ref
  ; network_sriov_requires_reboot: bool
  ; network_sriov_configuration_mode: [`sysfs | `modprobe | `unknown] }

and tunnel_t =
  { tunnel_uuid: string
  ; tunnel_access_PIF: pIF_t Rpc.Types.ref
  ; tunnel_transport_PIF: pIF_t Rpc.Types.ref
  ; tunnel_status: (string * string) list
  ; tunnel_other_config: (string * string) list }

and secret_t =
  { secret_uuid: string
  ; secret_value: string
  ; secret_other_config: (string * string) list }

and blob_t =
  { blob_uuid: string
  ; blob_name_label: string
  ; blob_name_description: string
  ; blob_size: int64
  ; blob_public: bool
  ; blob_last_updated: string
  ; blob_mime_type: string }

and console_t =
  { console_uuid: string
  ; console_protocol: [`vt100 | `rfb | `rdp]
  ; console_location: string
  ; console_VM: vM_t Rpc.Types.ref
  ; console_other_config: (string * string) list
  ; console_port: int64 }

and crashdump_t =
  { crashdump_uuid: string
  ; crashdump_VM: vM_t Rpc.Types.ref
  ; crashdump_VDI: vDI_t Rpc.Types.ref
  ; crashdump_other_config: (string * string) list }

and pBD_t =
  { pBD_uuid: string
  ; pBD_host: host_t Rpc.Types.ref
  ; pBD_SR: sR_t Rpc.Types.ref
  ; pBD_device_config: (string * string) list
  ; pBD_currently_attached: bool
  ; pBD_other_config: (string * string) list }

and vBD_metrics_t =
  { vBD_metrics_uuid: string
  ; vBD_metrics_io_read_kbs: float
  ; vBD_metrics_io_write_kbs: float
  ; vBD_metrics_last_updated: string
  ; vBD_metrics_other_config: (string * string) list }

and vBD_t =
  { vBD_uuid: string
  ; vBD_allowed_operations:
      [ `attach
      | `eject
      | `insert
      | `plug
      | `unplug
      | `unplug_force
      | `pause
      | `unpause ]
      list
  ; vBD_current_operations: (string * vbd_operations) list
  ; vBD_VM: vM_t Rpc.Types.ref
  ; vBD_VDI: vDI_t Rpc.Types.ref
  ; vBD_device: string
  ; vBD_userdevice: string
  ; vBD_bootable: bool
  ; vBD_mode: [`RO | `RW]
  ; vBD_type: [`CD | `Disk | `Floppy]
  ; vBD_unpluggable: bool
  ; vBD_storage_lock: bool
  ; vBD_empty: bool
  ; vBD_reserved: bool
  ; vBD_other_config: (string * string) list
  ; vBD_currently_attached: bool
  ; vBD_status_code: int64
  ; vBD_status_detail: string
  ; vBD_runtime_properties: (string * string) list
  ; vBD_qos_algorithm_type: string
  ; vBD_qos_algorithm_params: (string * string) list
  ; vBD_qos_supported_algorithms: string list
  ; vBD_metrics: vBD_metrics_t Rpc.Types.ref }

and vDI_t =
  { vDI_uuid: string
  ; vDI_name_label: string
  ; vDI_name_description: string
  ; vDI_allowed_operations:
      [ `clone
      | `copy
      | `resize
      | `resize_online
      | `snapshot
      | `mirror
      | `destroy
      | `forget
      | `update
      | `force_unlock
      | `generate_config
      | `enable_cbt
      | `disable_cbt
      | `data_destroy
      | `list_changed_blocks
      | `set_on_boot
      | `blocked ]
      list
  ; vDI_current_operations: (string * vdi_operations) list
  ; vDI_SR: sR_t Rpc.Types.ref
  ; vDI_VBDs: vBD_t Rpc.Types.ref list
  ; vDI_crash_dumps: crashdump_t Rpc.Types.ref list
  ; vDI_virtual_size: int64
  ; vDI_physical_utilisation: int64
  ; vDI_type:
      [ `system
      | `user
      | `ephemeral
      | `suspend
      | `crashdump
      | `ha_statefile
      | `metadata
      | `redo_log
      | `rrd
      | `pvs_cache
      | `cbt_metadata ]
  ; vDI_sharable: bool
  ; vDI_read_only: bool
  ; vDI_other_config: (string * string) list
  ; vDI_storage_lock: bool
  ; vDI_location: string
  ; vDI_managed: bool
  ; vDI_missing: bool
  ; vDI_parent: vDI_t Rpc.Types.ref
  ; vDI_xenstore_data: (string * string) list
  ; vDI_sm_config: (string * string) list
  ; vDI_is_a_snapshot: bool
  ; vDI_snapshot_of: vDI_t Rpc.Types.ref
  ; vDI_snapshots: vDI_t Rpc.Types.ref list
  ; vDI_snapshot_time: string
  ; vDI_tags: string list
  ; vDI_allow_caching: bool
  ; vDI_on_boot: [`reset | `persist]
  ; vDI_metadata_of_pool: pool_t Rpc.Types.ref
  ; vDI_metadata_latest: bool
  ; vDI_is_tools_iso: bool
  ; vDI_cbt_enabled: bool }

and sR_t =
  { sR_uuid: string
  ; sR_name_label: string
  ; sR_name_description: string
  ; sR_allowed_operations:
      [ `scan
      | `destroy
      | `forget
      | `plug
      | `unplug
      | `update
      | `vdi_create
      | `vdi_introduce
      | `vdi_destroy
      | `vdi_resize
      | `vdi_clone
      | `vdi_snapshot
      | `vdi_mirror
      | `vdi_enable_cbt
      | `vdi_disable_cbt
      | `vdi_data_destroy
      | `vdi_list_changed_blocks
      | `vdi_set_on_boot
      | `pbd_create
      | `pbd_destroy ]
      list
  ; sR_current_operations: (string * storage_operations) list
  ; sR_VDIs: vDI_t Rpc.Types.ref list
  ; sR_PBDs: pBD_t Rpc.Types.ref list
  ; sR_virtual_allocation: int64
  ; sR_physical_utilisation: int64
  ; sR_physical_size: int64
  ; sR_type: string
  ; sR_content_type: string
  ; sR_shared: bool
  ; sR_other_config: (string * string) list
  ; sR_tags: string list
  ; sR_default_vdi_visibility: bool
  ; sR_sm_config: (string * string) list
  ; sR_blobs: (string * blob_ref) list
  ; sR_local_cache_enabled: bool
  ; sR_introduced_by: dR_task_t Rpc.Types.ref
  ; sR_clustered: bool
  ; sR_is_tools_sr: bool }

and sM_t =
  { sM_uuid: string
  ; sM_name_label: string
  ; sM_name_description: string
  ; sM_type: string
  ; sM_vendor: string
  ; sM_copyright: string
  ; sM_version: string
  ; sM_required_api_version: string
  ; sM_configuration: (string * string) list
  ; sM_capabilities: string list
  ; sM_features: (string * int64) list
  ; sM_other_config: (string * string) list
  ; sM_driver_filename: string
  ; sM_required_cluster_stack: string list }

and vLAN_t =
  { vLAN_uuid: string
  ; vLAN_tagged_PIF: pIF_t Rpc.Types.ref
  ; vLAN_untagged_PIF: pIF_t Rpc.Types.ref
  ; vLAN_tag: int64
  ; vLAN_other_config: (string * string) list }

and bond_t =
  { bond_uuid: string
  ; bond_master: pIF_t Rpc.Types.ref
  ; bond_slaves: pIF_t Rpc.Types.ref list
  ; bond_other_config: (string * string) list
  ; bond_primary_slave: pIF_t Rpc.Types.ref
  ; bond_mode: [`balanceslb | `activebackup | `lacp]
  ; bond_properties: (string * string) list
  ; bond_links_up: int64 }

and pIF_metrics_t =
  { pIF_metrics_uuid: string
  ; pIF_metrics_io_read_kbs: float
  ; pIF_metrics_io_write_kbs: float
  ; pIF_metrics_carrier: bool
  ; pIF_metrics_vendor_id: string
  ; pIF_metrics_vendor_name: string
  ; pIF_metrics_device_id: string
  ; pIF_metrics_device_name: string
  ; pIF_metrics_speed: int64
  ; pIF_metrics_duplex: bool
  ; pIF_metrics_pci_bus_path: string
  ; pIF_metrics_last_updated: string
  ; pIF_metrics_other_config: (string * string) list }

and pIF_t =
  { pIF_uuid: string
  ; pIF_device: string
  ; pIF_network: network_t Rpc.Types.ref
  ; pIF_host: host_t Rpc.Types.ref
  ; pIF_MAC: string
  ; pIF_MTU: int64
  ; pIF_VLAN: int64
  ; pIF_device_name: string
  ; pIF_metrics: pIF_metrics_t Rpc.Types.ref
  ; pIF_physical: bool
  ; pIF_currently_attached: bool
  ; pIF_ip_configuration_mode: [`None | `DHCP | `Static]
  ; pIF_IP: string
  ; pIF_netmask: string
  ; pIF_gateway: string
  ; pIF_DNS: string
  ; pIF_bond_slave_of: bond_t Rpc.Types.ref
  ; pIF_bond_master_of: bond_t Rpc.Types.ref list
  ; pIF_VLAN_master_of: vLAN_t Rpc.Types.ref
  ; pIF_VLAN_slave_of: vLAN_t Rpc.Types.ref list
  ; pIF_management: bool
  ; pIF_other_config: (string * string) list
  ; pIF_disallow_unplug: bool
  ; pIF_tunnel_access_PIF_of: tunnel_t Rpc.Types.ref list
  ; pIF_tunnel_transport_PIF_of: tunnel_t Rpc.Types.ref list
  ; pIF_ipv6_configuration_mode: [`None | `DHCP | `Static | `Autoconf]
  ; pIF_IPv6: string list
  ; pIF_ipv6_gateway: string
  ; pIF_primary_address_type: [`IPv4 | `IPv6]
  ; pIF_managed: bool
  ; pIF_properties: (string * string) list
  ; pIF_capabilities: string list
  ; pIF_igmp_snooping_status: [`enabled | `disabled | `unknown]
  ; pIF_sriov_physical_PIF_of: network_sriov_t Rpc.Types.ref list
  ; pIF_sriov_logical_PIF_of: network_sriov_t Rpc.Types.ref list
  ; pIF_PCI: pCI_t Rpc.Types.ref }

and vIF_metrics_t =
  { vIF_metrics_uuid: string
  ; vIF_metrics_io_read_kbs: float
  ; vIF_metrics_io_write_kbs: float
  ; vIF_metrics_last_updated: string
  ; vIF_metrics_other_config: (string * string) list }

and vIF_t =
  { vIF_uuid: string
  ; vIF_allowed_operations: [`attach | `plug | `unplug] list
  ; vIF_current_operations: (string * vif_operations) list
  ; vIF_device: string
  ; vIF_network: network_t Rpc.Types.ref
  ; vIF_VM: vM_t Rpc.Types.ref
  ; vIF_MAC: string
  ; vIF_MTU: int64
  ; vIF_reserved: bool
  ; vIF_other_config: (string * string) list
  ; vIF_currently_attached: bool
  ; vIF_status_code: int64
  ; vIF_status_detail: string
  ; vIF_runtime_properties: (string * string) list
  ; vIF_qos_algorithm_type: string
  ; vIF_qos_algorithm_params: (string * string) list
  ; vIF_qos_supported_algorithms: string list
  ; vIF_metrics: vIF_metrics_t Rpc.Types.ref
  ; vIF_MAC_autogenerated: bool
  ; vIF_locking_mode: [`network_default | `locked | `unlocked | `disabled]
  ; vIF_ipv4_allowed: string list
  ; vIF_ipv6_allowed: string list
  ; vIF_ipv4_configuration_mode: [`None | `Static]
  ; vIF_ipv4_addresses: string list
  ; vIF_ipv4_gateway: string
  ; vIF_ipv6_configuration_mode: [`None | `Static]
  ; vIF_ipv6_addresses: string list
  ; vIF_ipv6_gateway: string
  ; vIF_reserved_pci: pCI_t Rpc.Types.ref }

and network_t =
  { network_uuid: string
  ; network_name_label: string
  ; network_name_description: string
  ; network_allowed_operations: [`attaching] list
  ; network_current_operations: (string * network_operations) list
  ; network_VIFs: vIF_t Rpc.Types.ref list
  ; network_PIFs: pIF_t Rpc.Types.ref list
  ; network_MTU: int64
  ; network_other_config: (string * string) list
  ; network_bridge: string
  ; network_managed: bool
  ; network_blobs: (string * blob_ref) list
  ; network_tags: string list
  ; network_default_locking_mode: [`unlocked | `disabled]
  ; network_assigned_ips: (vIF_ref * string) list
  ; network_purpose: [`nbd | `insecure_nbd] list }

and host_cpu_t =
  { host_cpu_uuid: string
  ; host_cpu_host: host_t Rpc.Types.ref
  ; host_cpu_number: int64
  ; host_cpu_vendor: string
  ; host_cpu_speed: int64
  ; host_cpu_modelname: string
  ; host_cpu_family: int64
  ; host_cpu_model: int64
  ; host_cpu_stepping: string
  ; host_cpu_flags: string
  ; host_cpu_features: string
  ; host_cpu_utilisation: float
  ; host_cpu_other_config: (string * string) list }

and host_metrics_t =
  { host_metrics_uuid: string
  ; host_metrics_memory_total: int64
  ; host_metrics_memory_free: int64
  ; host_metrics_live: bool
  ; host_metrics_last_updated: string
  ; host_metrics_other_config: (string * string) list }

and host_patch_t =
  { host_patch_uuid: string
  ; host_patch_name_label: string
  ; host_patch_name_description: string
  ; host_patch_version: string
  ; host_patch_host: host_t Rpc.Types.ref
  ; host_patch_filename: string
  ; host_patch_applied: bool
  ; host_patch_timestamp_applied: string
  ; host_patch_size: int64
  ; host_patch_pool_patch: pool_patch_t Rpc.Types.ref
  ; host_patch_other_config: (string * string) list }

and host_crashdump_t =
  { host_crashdump_uuid: string
  ; host_crashdump_host: host_t Rpc.Types.ref
  ; host_crashdump_timestamp: string
  ; host_crashdump_size: int64
  ; host_crashdump_filename: string
  ; host_crashdump_other_config: (string * string) list }

and host_t =
  { host_uuid: string
  ; host_name_label: string
  ; host_name_description: string
  ; host_memory_overhead: int64
  ; host_allowed_operations:
      [ `provision
      | `evacuate
      | `shutdown
      | `reboot
      | `power_on
      | `vm_start
      | `vm_resume
      | `vm_migrate ]
      list
  ; host_current_operations: (string * host_allowed_operations) list
  ; host_API_version_major: int64
  ; host_API_version_minor: int64
  ; host_API_version_vendor: string
  ; host_API_version_vendor_implementation: (string * string) list
  ; host_enabled: bool
  ; host_software_version: (string * string) list
  ; host_other_config: (string * string) list
  ; host_capabilities: string list
  ; host_cpu_configuration: (string * string) list
  ; host_sched_policy: string
  ; host_supported_bootloaders: string list
  ; host_resident_VMs: vM_t Rpc.Types.ref list
  ; host_logging: (string * string) list
  ; host_PIFs: pIF_t Rpc.Types.ref list
  ; host_suspend_image_sr: sR_t Rpc.Types.ref
  ; host_crash_dump_sr: sR_t Rpc.Types.ref
  ; host_crashdumps: host_crashdump_t Rpc.Types.ref list
  ; host_patches: host_patch_t Rpc.Types.ref list
  ; host_updates: pool_update_t Rpc.Types.ref list
  ; host_PBDs: pBD_t Rpc.Types.ref list
  ; host_host_CPUs: host_cpu_t Rpc.Types.ref list
  ; host_cpu_info: (string * string) list
  ; host_hostname: string
  ; host_address: string
  ; host_metrics: host_metrics_t Rpc.Types.ref
  ; host_license_params: (string * string) list
  ; host_boot_free_mem: int64
  ; host_ha_statefiles: string list
  ; host_ha_network_peers: string list
  ; host_blobs: (string * blob_ref) list
  ; host_tags: string list
  ; host_external_auth_type: string
  ; host_external_auth_service_name: string
  ; host_external_auth_configuration: (string * string) list
  ; host_edition: string
  ; host_license_server: (string * string) list
  ; host_bios_strings: (string * string) list
  ; host_power_on_mode: string
  ; host_power_on_config: (string * string) list
  ; host_local_cache_sr: sR_t Rpc.Types.ref
  ; host_chipset_info: (string * string) list
  ; host_PCIs: pCI_t Rpc.Types.ref list
  ; host_PGPUs: pGPU_t Rpc.Types.ref list
  ; host_PUSBs: pUSB_t Rpc.Types.ref list
  ; host_ssl_legacy: bool
  ; host_guest_VCPUs_params: (string * string) list
  ; host_display:
      [`enabled | `disable_on_reboot | `disabled | `enable_on_reboot]
  ; host_virtual_hardware_platform_versions: int64 list
  ; host_control_domain: vM_t Rpc.Types.ref
  ; host_updates_requiring_reboot: pool_update_t Rpc.Types.ref list
  ; host_features: feature_t Rpc.Types.ref list
  ; host_iscsi_iqn: string
  ; host_multipathing: bool }

and dR_task_t =
  {dR_task_uuid: string; dR_task_introduced_SRs: sR_t Rpc.Types.ref list}

and vM_appliance_t =
  { vM_appliance_uuid: string
  ; vM_appliance_name_label: string
  ; vM_appliance_name_description: string
  ; vM_appliance_allowed_operations:
      [`start | `clean_shutdown | `hard_shutdown | `shutdown] list
  ; vM_appliance_current_operations: (string * vm_appliance_operation) list
  ; vM_appliance_VMs: vM_t Rpc.Types.ref list }

and vMSS_t =
  { vMSS_uuid: string
  ; vMSS_name_label: string
  ; vMSS_name_description: string
  ; vMSS_enabled: bool
  ; vMSS_type: [`snapshot | `checkpoint | `snapshot_with_quiesce]
  ; vMSS_retained_snapshots: int64
  ; vMSS_frequency: [`hourly | `daily | `weekly]
  ; vMSS_schedule: (string * string) list
  ; vMSS_last_run_time: string
  ; vMSS_VMs: vM_t Rpc.Types.ref list }

and vMPP_t =
  { vMPP_uuid: string
  ; vMPP_name_label: string
  ; vMPP_name_description: string
  ; vMPP_is_policy_enabled: bool
  ; vMPP_backup_type: [`snapshot | `checkpoint]
  ; vMPP_backup_retention_value: int64
  ; vMPP_backup_frequency: [`hourly | `daily | `weekly]
  ; vMPP_backup_schedule: (string * string) list
  ; vMPP_is_backup_running: bool
  ; vMPP_backup_last_run_time: string
  ; vMPP_archive_target_type: [`none | `cifs | `nfs]
  ; vMPP_archive_target_config: (string * string) list
  ; vMPP_archive_frequency: [`never | `always_after_backup | `daily | `weekly]
  ; vMPP_archive_schedule: (string * string) list
  ; vMPP_is_archive_running: bool
  ; vMPP_archive_last_run_time: string
  ; vMPP_VMs: vM_t Rpc.Types.ref list
  ; vMPP_is_alarm_enabled: bool
  ; vMPP_alarm_config: (string * string) list
  ; vMPP_recent_alerts: string list }

and vM_guest_metrics_t =
  { vM_guest_metrics_uuid: string
  ; vM_guest_metrics_os_version: (string * string) list
  ; vM_guest_metrics_PV_drivers_version: (string * string) list
  ; vM_guest_metrics_PV_drivers_up_to_date: bool
  ; vM_guest_metrics_memory: (string * string) list
  ; vM_guest_metrics_disks: (string * string) list
  ; vM_guest_metrics_networks: (string * string) list
  ; vM_guest_metrics_other: (string * string) list
  ; vM_guest_metrics_last_updated: string
  ; vM_guest_metrics_other_config: (string * string) list
  ; vM_guest_metrics_live: bool
  ; vM_guest_metrics_can_use_hotplug_vbd: [`yes | `no | `unspecified]
  ; vM_guest_metrics_can_use_hotplug_vif: [`yes | `no | `unspecified]
  ; vM_guest_metrics_PV_drivers_detected: bool }

and vM_metrics_t =
  { vM_metrics_uuid: string
  ; vM_metrics_memory_actual: int64
  ; vM_metrics_VCPUs_number: int64
  ; vM_metrics_VCPUs_utilisation: (int64 * float) list
  ; vM_metrics_VCPUs_CPU: (int64 * int64) list
  ; vM_metrics_VCPUs_params: (string * string) list
  ; vM_metrics_VCPUs_flags: (int64 * string list) list
  ; vM_metrics_state: string list
  ; vM_metrics_start_time: string
  ; vM_metrics_install_time: string
  ; vM_metrics_last_updated: string
  ; vM_metrics_other_config: (string * string) list
  ; vM_metrics_hvm: bool
  ; vM_metrics_nested_virt: bool
  ; vM_metrics_nomigrate: bool
  ; vM_metrics_current_domain_type: [`hvm | `pv | `pv_in_pvh | `unspecified] }

and vM_t =
  { vM_uuid: string
  ; vM_allowed_operations:
      [ `snapshot
      | `clone
      | `copy
      | `create_template
      | `revert
      | `checkpoint
      | `snapshot_with_quiesce
      | `provision
      | `start
      | `start_on
      | `pause
      | `unpause
      | `clean_shutdown
      | `clean_reboot
      | `hard_shutdown
      | `power_state_reset
      | `hard_reboot
      | `suspend
      | `csvm
      | `resume
      | `resume_on
      | `pool_migrate
      | `migrate_send
      | `get_boot_record
      | `send_sysrq
      | `send_trigger
      | `query_services
      | `shutdown
      | `call_plugin
      | `changing_memory_live
      | `awaiting_memory_live
      | `changing_dynamic_range
      | `changing_static_range
      | `changing_memory_limits
      | `changing_shadow_memory
      | `changing_shadow_memory_live
      | `changing_VCPUs
      | `changing_VCPUs_live
      | `changing_NVRAM
      | `assert_operation_valid
      | `data_source_op
      | `update_allowed_operations
      | `make_into_template
      | `import
      | `export
      | `metadata_export
      | `reverting
      | `destroy ]
      list
  ; vM_current_operations: (string * vm_operations) list
  ; vM_power_state: [`Halted | `Paused | `Running | `Suspended]
  ; vM_name_label: string
  ; vM_name_description: string
  ; vM_user_version: int64
  ; vM_is_a_template: bool
  ; vM_is_default_template: bool
  ; vM_suspend_VDI: vDI_t Rpc.Types.ref
  ; vM_resident_on: host_t Rpc.Types.ref
  ; vM_scheduled_to_be_resident_on: host_t Rpc.Types.ref
  ; vM_affinity: host_t Rpc.Types.ref
  ; vM_memory_overhead: int64
  ; vM_memory_target: int64
  ; vM_memory_static_max: int64
  ; vM_memory_dynamic_max: int64
  ; vM_memory_dynamic_min: int64
  ; vM_memory_static_min: int64
  ; vM_VCPUs_params: (string * string) list
  ; vM_VCPUs_max: int64
  ; vM_VCPUs_at_startup: int64
  ; vM_actions_after_shutdown: [`destroy | `restart]
  ; vM_actions_after_reboot: [`destroy | `restart]
  ; vM_actions_after_crash:
      [ `destroy
      | `coredump_and_destroy
      | `restart
      | `coredump_and_restart
      | `preserve
      | `rename_restart ]
  ; vM_consoles: console_t Rpc.Types.ref list
  ; vM_VIFs: vIF_t Rpc.Types.ref list
  ; vM_VBDs: vBD_t Rpc.Types.ref list
  ; vM_VUSBs: vUSB_t Rpc.Types.ref list
  ; vM_crash_dumps: crashdump_t Rpc.Types.ref list
  ; vM_VTPMs: vTPM_t Rpc.Types.ref list
  ; vM_PV_bootloader: string
  ; vM_PV_kernel: string
  ; vM_PV_ramdisk: string
  ; vM_PV_args: string
  ; vM_PV_bootloader_args: string
  ; vM_PV_legacy_args: string
  ; vM_HVM_boot_policy: string
  ; vM_HVM_boot_params: (string * string) list
  ; vM_HVM_shadow_multiplier: float
  ; vM_platform: (string * string) list
  ; vM_PCI_bus: string
  ; vM_other_config: (string * string) list
  ; vM_domid: int64
  ; vM_domarch: string
  ; vM_last_boot_CPU_flags: (string * string) list
  ; vM_is_control_domain: bool
  ; vM_metrics: vM_metrics_t Rpc.Types.ref
  ; vM_guest_metrics: vM_guest_metrics_t Rpc.Types.ref
  ; vM_last_booted_record: string
  ; vM_recommendations: string
  ; vM_xenstore_data: (string * string) list
  ; vM_ha_always_run: bool
  ; vM_ha_restart_priority: string
  ; vM_is_a_snapshot: bool
  ; vM_snapshot_of: vM_t Rpc.Types.ref
  ; vM_snapshots: vM_t Rpc.Types.ref list
  ; vM_snapshot_time: string
  ; vM_transportable_snapshot_id: string
  ; vM_blobs: (string * blob_ref) list
  ; vM_tags: string list
  ; vM_blocked_operations: (vm_operations * string) list
  ; vM_snapshot_info: (string * string) list
  ; vM_snapshot_metadata: string
  ; vM_parent: vM_t Rpc.Types.ref
  ; vM_children: vM_t Rpc.Types.ref list
  ; vM_bios_strings: (string * string) list
  ; vM_protection_policy: vMPP_t Rpc.Types.ref
  ; vM_is_snapshot_from_vmpp: bool
  ; vM_snapshot_schedule: vMSS_t Rpc.Types.ref
  ; vM_is_vmss_snapshot: bool
  ; vM_appliance: vM_appliance_t Rpc.Types.ref
  ; vM_start_delay: int64
  ; vM_shutdown_delay: int64
  ; vM_order: int64
  ; vM_VGPUs: vGPU_t Rpc.Types.ref list
  ; vM_attached_PCIs: pCI_t Rpc.Types.ref list
  ; vM_suspend_SR: sR_t Rpc.Types.ref
  ; vM_version: int64
  ; vM_generation_id: string
  ; vM_hardware_platform_version: int64
  ; vM_has_vendor_device: bool
  ; vM_requires_reboot: bool
  ; vM_reference_label: string
  ; vM_domain_type: [`hvm | `pv | `pv_in_pvh | `unspecified]
  ; vM_NVRAM: (string * string) list }

and pool_update_t =
  { pool_update_uuid: string
  ; pool_update_name_label: string
  ; pool_update_name_description: string
  ; pool_update_version: string
  ; pool_update_installation_size: int64
  ; pool_update_key: string
  ; pool_update_after_apply_guidance:
      [`restartHVM | `restartPV | `restartHost | `restartXAPI] list
  ; pool_update_vdi: vDI_t Rpc.Types.ref
  ; pool_update_hosts: host_t Rpc.Types.ref list
  ; pool_update_other_config: (string * string) list
  ; pool_update_enforce_homogeneity: bool }

and pool_patch_t =
  { pool_patch_uuid: string
  ; pool_patch_name_label: string
  ; pool_patch_name_description: string
  ; pool_patch_version: string
  ; pool_patch_filename: string
  ; pool_patch_size: int64
  ; pool_patch_pool_applied: bool
  ; pool_patch_host_patches: host_patch_t Rpc.Types.ref list
  ; pool_patch_after_apply_guidance:
      [`restartHVM | `restartPV | `restartHost | `restartXAPI] list
  ; pool_patch_pool_update: pool_update_t Rpc.Types.ref
  ; pool_patch_other_config: (string * string) list }

and pool_t =
  { pool_uuid: string
  ; pool_name_label: string
  ; pool_name_description: string
  ; pool_master: host_t Rpc.Types.ref
  ; pool_default_SR: sR_t Rpc.Types.ref
  ; pool_suspend_image_SR: sR_t Rpc.Types.ref
  ; pool_crash_dump_SR: sR_t Rpc.Types.ref
  ; pool_other_config: (string * string) list
  ; pool_ha_enabled: bool
  ; pool_ha_configuration: (string * string) list
  ; pool_ha_statefiles: string list
  ; pool_ha_host_failures_to_tolerate: int64
  ; pool_ha_plan_exists_for: int64
  ; pool_ha_allow_overcommit: bool
  ; pool_ha_overcommitted: bool
  ; pool_blobs: (string * blob_ref) list
  ; pool_tags: string list
  ; pool_gui_config: (string * string) list
  ; pool_health_check_config: (string * string) list
  ; pool_wlb_url: string
  ; pool_wlb_username: string
  ; pool_wlb_password: secret_t Rpc.Types.ref
  ; pool_wlb_enabled: bool
  ; pool_wlb_verify_cert: bool
  ; pool_redo_log_enabled: bool
  ; pool_redo_log_vdi: vDI_t Rpc.Types.ref
  ; pool_vswitch_controller: string
  ; pool_restrictions: (string * string) list
  ; pool_metadata_VDIs: vDI_t Rpc.Types.ref list
  ; pool_ha_cluster_stack: string
  ; pool_allowed_operations: [`ha_enable | `ha_disable | `cluster_create] list
  ; pool_current_operations: (string * pool_allowed_operations) list
  ; pool_guest_agent_config: (string * string) list
  ; pool_cpu_info: (string * string) list
  ; pool_policy_no_vendor_device: bool
  ; pool_live_patching_disabled: bool
  ; pool_igmp_snooping_enabled: bool }

and task_t =
  { task_uuid: string
  ; task_name_label: string
  ; task_name_description: string
  ; task_allowed_operations: [`cancel | `destroy] list
  ; task_current_operations: (string * task_allowed_operations) list
  ; task_created: string
  ; task_finished: string
  ; task_status: [`pending | `success | `failure | `cancelling | `cancelled]
  ; task_session: session_t Rpc.Types.ref
  ; task_resident_on: host_t Rpc.Types.ref
  ; task_progress: float
  ; task_externalpid: int64
  ; task_stunnelpid: int64
  ; task_forwarded: bool
  ; task_forwarded_to: host_t Rpc.Types.ref
  ; task_type: string
  ; task_result: string
  ; task_error_info: string list
  ; task_other_config: (string * string) list
  ; task_subtask_of: task_t Rpc.Types.ref
  ; task_subtasks: task_t Rpc.Types.ref list
  ; task_backtrace: string }

and role_t =
  { role_uuid: string
  ; role_name_label: string
  ; role_name_description: string
  ; role_subroles: role_t Rpc.Types.ref list }

and subject_t =
  { subject_uuid: string
  ; subject_subject_identifier: string
  ; subject_other_config: (string * string) list
  ; subject_roles: role_t Rpc.Types.ref list }

and event_t =
  { event_id: int64
  ; event_timestamp: string
  ; event_class: string
  ; event_operation: [`add | `del | `_mod]
  ; event_ref: string
  ; event_obj_uuid: string }

and data_source_t =
  { data_source_name_label: string
  ; data_source_name_description: string
  ; data_source_enabled: bool
  ; data_source_standard: bool
  ; data_source_units: string
  ; data_source_min: float
  ; data_source_max: float
  ; data_source_value: float }

and probe_result_t =
  { probe_result_configuration: (string * string) list
  ; probe_result_complete: bool
  ; probe_result_sr: sr_stat_t option
  ; probe_result_extra_info: (string * string) list }

and vdi_nbd_server_info_t =
  { vdi_nbd_server_info_exportname: string
  ; vdi_nbd_server_info_address: string
  ; vdi_nbd_server_info_port: int64
  ; vdi_nbd_server_info_cert: string
  ; vdi_nbd_server_info_subject: string }

and message_t =
  { message_uuid: string
  ; message_name: string
  ; message_priority: int64
  ; message_cls: [`VM | `Host | `SR | `Pool | `VMPP | `VMSS | `PVS_proxy | `VDI]
  ; message_obj_uuid: string
  ; message_timestamp: string
  ; message_body: string }

and user_t =
  { user_uuid: string
  ; user_short_name: string
  ; user_fullname: string
  ; user_other_config: (string * string) list }

and vTPM_t =
  { vTPM_uuid: string
  ; vTPM_VM: vM_t Rpc.Types.ref
  ; vTPM_backend: vM_t Rpc.Types.ref }

and lVHD_t = {lVHD_uuid: string}

and session_t =
  { session_uuid: string
  ; session_this_host: host_t Rpc.Types.ref
  ; session_this_user: user_t Rpc.Types.ref
  ; session_last_active: string
  ; session_pool: bool
  ; session_other_config: (string * string) list
  ; session_is_local_superuser: bool
  ; session_subject: subject_t Rpc.Types.ref
  ; session_validation_time: string
  ; session_auth_user_sid: string
  ; session_auth_user_name: string
  ; session_rbac_permissions: string list
  ; session_tasks: task_t Rpc.Types.ref list
  ; session_parent: session_t Rpc.Types.ref
  ; session_originator: string }
[@@deriving rpcty]

module Cluster_host = struct
  let uuid = cluster_host_t_cluster_host_uuid

  let cluster = cluster_host_t_cluster_host_cluster

  let host = cluster_host_t_cluster_host_host

  let enabled = cluster_host_t_cluster_host_enabled

  let pIF = cluster_host_t_cluster_host_PIF

  let joined = cluster_host_t_cluster_host_joined

  let allowed_operations = cluster_host_t_cluster_host_allowed_operations

  let current_operations = cluster_host_t_cluster_host_current_operations

  let other_config = cluster_host_t_cluster_host_other_config
end

module Cluster = struct
  let uuid = cluster_t_cluster_uuid

  let cluster_hosts = cluster_t_cluster_cluster_hosts

  let pending_forget = cluster_t_cluster_pending_forget

  let cluster_token = cluster_t_cluster_cluster_token

  let cluster_stack = cluster_t_cluster_cluster_stack

  let allowed_operations = cluster_t_cluster_allowed_operations

  let current_operations = cluster_t_cluster_current_operations

  let pool_auto_join = cluster_t_cluster_pool_auto_join

  let token_timeout = cluster_t_cluster_token_timeout

  let token_timeout_coefficient = cluster_t_cluster_token_timeout_coefficient

  let cluster_config = cluster_t_cluster_cluster_config

  let other_config = cluster_t_cluster_other_config
end

module VUSB = struct
  let uuid = vUSB_t_vUSB_uuid

  let allowed_operations = vUSB_t_vUSB_allowed_operations

  let current_operations = vUSB_t_vUSB_current_operations

  let vM = vUSB_t_vUSB_VM

  let uSB_group = vUSB_t_vUSB_USB_group

  let other_config = vUSB_t_vUSB_other_config

  let currently_attached = vUSB_t_vUSB_currently_attached
end

module USB_group = struct
  let uuid = uSB_group_t_uSB_group_uuid

  let name_label = uSB_group_t_uSB_group_name_label

  let name_description = uSB_group_t_uSB_group_name_description

  let pUSBs = uSB_group_t_uSB_group_PUSBs

  let vUSBs = uSB_group_t_uSB_group_VUSBs

  let other_config = uSB_group_t_uSB_group_other_config
end

module PUSB = struct
  let uuid = pUSB_t_pUSB_uuid

  let uSB_group = pUSB_t_pUSB_USB_group

  let host = pUSB_t_pUSB_host

  let path = pUSB_t_pUSB_path

  let vendor_id = pUSB_t_pUSB_vendor_id

  let vendor_desc = pUSB_t_pUSB_vendor_desc

  let product_id = pUSB_t_pUSB_product_id

  let product_desc = pUSB_t_pUSB_product_desc

  let serial = pUSB_t_pUSB_serial

  let version = pUSB_t_pUSB_version

  let description = pUSB_t_pUSB_description

  let passthrough_enabled = pUSB_t_pUSB_passthrough_enabled

  let other_config = pUSB_t_pUSB_other_config
end

module SDN_controller = struct
  let uuid = sDN_controller_t_sDN_controller_uuid

  let protocol = sDN_controller_t_sDN_controller_protocol

  let address = sDN_controller_t_sDN_controller_address

  let port = sDN_controller_t_sDN_controller_port
end

module Feature = struct
  let uuid = feature_t_feature_uuid

  let name_label = feature_t_feature_name_label

  let name_description = feature_t_feature_name_description

  let enabled = feature_t_feature_enabled

  let experimental = feature_t_feature_experimental

  let version = feature_t_feature_version

  let host = feature_t_feature_host
end

module PVS_cache_storage = struct
  let uuid = pVS_cache_storage_t_pVS_cache_storage_uuid

  let host = pVS_cache_storage_t_pVS_cache_storage_host

  let sR = pVS_cache_storage_t_pVS_cache_storage_SR

  let site = pVS_cache_storage_t_pVS_cache_storage_site

  let size = pVS_cache_storage_t_pVS_cache_storage_size

  let vDI = pVS_cache_storage_t_pVS_cache_storage_VDI
end

module PVS_proxy = struct
  let uuid = pVS_proxy_t_pVS_proxy_uuid

  let site = pVS_proxy_t_pVS_proxy_site

  let vIF = pVS_proxy_t_pVS_proxy_VIF

  let currently_attached = pVS_proxy_t_pVS_proxy_currently_attached

  let status = pVS_proxy_t_pVS_proxy_status
end

module PVS_server = struct
  let uuid = pVS_server_t_pVS_server_uuid

  let addresses = pVS_server_t_pVS_server_addresses

  let first_port = pVS_server_t_pVS_server_first_port

  let last_port = pVS_server_t_pVS_server_last_port

  let site = pVS_server_t_pVS_server_site
end

module PVS_site = struct
  let uuid = pVS_site_t_pVS_site_uuid

  let name_label = pVS_site_t_pVS_site_name_label

  let name_description = pVS_site_t_pVS_site_name_description

  let pVS_uuid = pVS_site_t_pVS_site_PVS_uuid

  let cache_storage = pVS_site_t_pVS_site_cache_storage

  let servers = pVS_site_t_pVS_site_servers

  let proxies = pVS_site_t_pVS_site_proxies
end

module VGPU_type = struct
  let uuid = vGPU_type_t_vGPU_type_uuid

  let vendor_name = vGPU_type_t_vGPU_type_vendor_name

  let model_name = vGPU_type_t_vGPU_type_model_name

  let framebuffer_size = vGPU_type_t_vGPU_type_framebuffer_size

  let max_heads = vGPU_type_t_vGPU_type_max_heads

  let max_resolution_x = vGPU_type_t_vGPU_type_max_resolution_x

  let max_resolution_y = vGPU_type_t_vGPU_type_max_resolution_y

  let size = vGPU_type_t_vGPU_type_size

  let supported_on_PGPUs = vGPU_type_t_vGPU_type_supported_on_PGPUs

  let enabled_on_PGPUs = vGPU_type_t_vGPU_type_enabled_on_PGPUs

  let vGPUs = vGPU_type_t_vGPU_type_VGPUs

  let internal_config = vGPU_type_t_vGPU_type_internal_config

  let supported_on_GPU_groups = vGPU_type_t_vGPU_type_supported_on_GPU_groups

  let enabled_on_GPU_groups = vGPU_type_t_vGPU_type_enabled_on_GPU_groups

  let implementation = vGPU_type_t_vGPU_type_implementation

  let identifier = vGPU_type_t_vGPU_type_identifier

  let experimental = vGPU_type_t_vGPU_type_experimental
end

module VGPU = struct
  let uuid = vGPU_t_vGPU_uuid

  let vM = vGPU_t_vGPU_VM

  let gPU_group = vGPU_t_vGPU_GPU_group

  let device = vGPU_t_vGPU_device

  let currently_attached = vGPU_t_vGPU_currently_attached

  let other_config = vGPU_t_vGPU_other_config

  let _type = vGPU_t_vGPU_type

  let resident_on = vGPU_t_vGPU_resident_on

  let scheduled_to_be_resident_on = vGPU_t_vGPU_scheduled_to_be_resident_on

  let compatibility_metadata = vGPU_t_vGPU_compatibility_metadata
end

module GPU_group = struct
  let uuid = gPU_group_t_gPU_group_uuid

  let name_label = gPU_group_t_gPU_group_name_label

  let name_description = gPU_group_t_gPU_group_name_description

  let pGPUs = gPU_group_t_gPU_group_PGPUs

  let vGPUs = gPU_group_t_gPU_group_VGPUs

  let gPU_types = gPU_group_t_gPU_group_GPU_types

  let other_config = gPU_group_t_gPU_group_other_config

  let allocation_algorithm = gPU_group_t_gPU_group_allocation_algorithm

  let supported_VGPU_types = gPU_group_t_gPU_group_supported_VGPU_types

  let enabled_VGPU_types = gPU_group_t_gPU_group_enabled_VGPU_types
end

module PGPU = struct
  let uuid = pGPU_t_pGPU_uuid

  let pCI = pGPU_t_pGPU_PCI

  let gPU_group = pGPU_t_pGPU_GPU_group

  let host = pGPU_t_pGPU_host

  let other_config = pGPU_t_pGPU_other_config

  let supported_VGPU_types = pGPU_t_pGPU_supported_VGPU_types

  let enabled_VGPU_types = pGPU_t_pGPU_enabled_VGPU_types

  let resident_VGPUs = pGPU_t_pGPU_resident_VGPUs

  let size = pGPU_t_pGPU_size

  let supported_VGPU_max_capacities = pGPU_t_pGPU_supported_VGPU_max_capacities

  let dom0_access = pGPU_t_pGPU_dom0_access

  let is_system_display_device = pGPU_t_pGPU_is_system_display_device

  let compatibility_metadata = pGPU_t_pGPU_compatibility_metadata
end

module PCI = struct
  let uuid = pCI_t_pCI_uuid

  let class_id = pCI_t_pCI_class_id

  let class_name = pCI_t_pCI_class_name

  let vendor_id = pCI_t_pCI_vendor_id

  let vendor_name = pCI_t_pCI_vendor_name

  let device_id = pCI_t_pCI_device_id

  let device_name = pCI_t_pCI_device_name

  let host = pCI_t_pCI_host

  let pci_id = pCI_t_pCI_pci_id

  let functions = pCI_t_pCI_functions

  let virtual_functions = pCI_t_pCI_virtual_functions

  let physical_function = pCI_t_pCI_physical_function

  let attached_VMs = pCI_t_pCI_attached_VMs

  let dependencies = pCI_t_pCI_dependencies

  let other_config = pCI_t_pCI_other_config

  let subsystem_vendor_id = pCI_t_pCI_subsystem_vendor_id

  let subsystem_vendor_name = pCI_t_pCI_subsystem_vendor_name

  let subsystem_device_id = pCI_t_pCI_subsystem_device_id

  let subsystem_device_name = pCI_t_pCI_subsystem_device_name

  let scheduled_to_be_attached_to = pCI_t_pCI_scheduled_to_be_attached_to

  let driver_name = pCI_t_pCI_driver_name
end

module Network_sriov = struct
  let uuid = network_sriov_t_network_sriov_uuid

  let physical_PIF = network_sriov_t_network_sriov_physical_PIF

  let logical_PIF = network_sriov_t_network_sriov_logical_PIF

  let requires_reboot = network_sriov_t_network_sriov_requires_reboot

  let configuration_mode = network_sriov_t_network_sriov_configuration_mode
end

module Tunnel = struct
  let uuid = tunnel_t_tunnel_uuid

  let access_PIF = tunnel_t_tunnel_access_PIF

  let transport_PIF = tunnel_t_tunnel_transport_PIF

  let status = tunnel_t_tunnel_status

  let other_config = tunnel_t_tunnel_other_config
end

module Secret = struct
  let uuid = secret_t_secret_uuid

  let value = secret_t_secret_value

  let other_config = secret_t_secret_other_config
end

module Blob = struct
  let uuid = blob_t_blob_uuid

  let name_label = blob_t_blob_name_label

  let name_description = blob_t_blob_name_description

  let size = blob_t_blob_size

  let public = blob_t_blob_public

  let last_updated = blob_t_blob_last_updated

  let mime_type = blob_t_blob_mime_type
end

module User = struct
  let uuid = user_t_user_uuid

  let short_name = user_t_user_short_name

  let fullname = user_t_user_fullname

  let other_config = user_t_user_other_config
end

module Console = struct
  let uuid = console_t_console_uuid

  let protocol = console_t_console_protocol

  let location = console_t_console_location

  let vM = console_t_console_VM

  let other_config = console_t_console_other_config

  let port = console_t_console_port
end

module VTPM = struct
  let uuid = vTPM_t_vTPM_uuid

  let vM = vTPM_t_vTPM_VM

  let backend = vTPM_t_vTPM_backend
end

module Crashdump = struct
  let uuid = crashdump_t_crashdump_uuid

  let vM = crashdump_t_crashdump_VM

  let vDI = crashdump_t_crashdump_VDI

  let other_config = crashdump_t_crashdump_other_config
end

module PBD = struct
  let uuid = pBD_t_pBD_uuid

  let host = pBD_t_pBD_host

  let sR = pBD_t_pBD_SR

  let device_config = pBD_t_pBD_device_config

  let currently_attached = pBD_t_pBD_currently_attached

  let other_config = pBD_t_pBD_other_config
end

module VBD_metrics = struct
  let uuid = vBD_metrics_t_vBD_metrics_uuid

  let io_read_kbs = vBD_metrics_t_vBD_metrics_io_read_kbs

  let io_write_kbs = vBD_metrics_t_vBD_metrics_io_write_kbs

  let last_updated = vBD_metrics_t_vBD_metrics_last_updated

  let other_config = vBD_metrics_t_vBD_metrics_other_config
end

module VBD = struct
  let uuid = vBD_t_vBD_uuid

  let allowed_operations = vBD_t_vBD_allowed_operations

  let current_operations = vBD_t_vBD_current_operations

  let vM = vBD_t_vBD_VM

  let vDI = vBD_t_vBD_VDI

  let device = vBD_t_vBD_device

  let userdevice = vBD_t_vBD_userdevice

  let bootable = vBD_t_vBD_bootable

  let mode = vBD_t_vBD_mode

  let _type = vBD_t_vBD_type

  let unpluggable = vBD_t_vBD_unpluggable

  let storage_lock = vBD_t_vBD_storage_lock

  let empty = vBD_t_vBD_empty

  let reserved = vBD_t_vBD_reserved

  let other_config = vBD_t_vBD_other_config

  let currently_attached = vBD_t_vBD_currently_attached

  let status_code = vBD_t_vBD_status_code

  let status_detail = vBD_t_vBD_status_detail

  let runtime_properties = vBD_t_vBD_runtime_properties

  let qos_algorithm_type = vBD_t_vBD_qos_algorithm_type

  let qos_algorithm_params = vBD_t_vBD_qos_algorithm_params

  let qos_supported_algorithms = vBD_t_vBD_qos_supported_algorithms

  let metrics = vBD_t_vBD_metrics
end

module VDI = struct
  let uuid = vDI_t_vDI_uuid

  let name_label = vDI_t_vDI_name_label

  let name_description = vDI_t_vDI_name_description

  let allowed_operations = vDI_t_vDI_allowed_operations

  let current_operations = vDI_t_vDI_current_operations

  let sR = vDI_t_vDI_SR

  let vBDs = vDI_t_vDI_VBDs

  let crash_dumps = vDI_t_vDI_crash_dumps

  let virtual_size = vDI_t_vDI_virtual_size

  let physical_utilisation = vDI_t_vDI_physical_utilisation

  let _type = vDI_t_vDI_type

  let sharable = vDI_t_vDI_sharable

  let read_only = vDI_t_vDI_read_only

  let other_config = vDI_t_vDI_other_config

  let storage_lock = vDI_t_vDI_storage_lock

  let location = vDI_t_vDI_location

  let managed = vDI_t_vDI_managed

  let missing = vDI_t_vDI_missing

  let parent = vDI_t_vDI_parent

  let xenstore_data = vDI_t_vDI_xenstore_data

  let sm_config = vDI_t_vDI_sm_config

  let is_a_snapshot = vDI_t_vDI_is_a_snapshot

  let snapshot_of = vDI_t_vDI_snapshot_of

  let snapshots = vDI_t_vDI_snapshots

  let snapshot_time = vDI_t_vDI_snapshot_time

  let tags = vDI_t_vDI_tags

  let allow_caching = vDI_t_vDI_allow_caching

  let on_boot = vDI_t_vDI_on_boot

  let metadata_of_pool = vDI_t_vDI_metadata_of_pool

  let metadata_latest = vDI_t_vDI_metadata_latest

  let is_tools_iso = vDI_t_vDI_is_tools_iso

  let cbt_enabled = vDI_t_vDI_cbt_enabled
end

module LVHD = struct
  let uuid = lVHD_t_lVHD_uuid
end

module SR = struct
  let uuid = sR_t_sR_uuid

  let name_label = sR_t_sR_name_label

  let name_description = sR_t_sR_name_description

  let allowed_operations = sR_t_sR_allowed_operations

  let current_operations = sR_t_sR_current_operations

  let vDIs = sR_t_sR_VDIs

  let pBDs = sR_t_sR_PBDs

  let virtual_allocation = sR_t_sR_virtual_allocation

  let physical_utilisation = sR_t_sR_physical_utilisation

  let physical_size = sR_t_sR_physical_size

  let _type = sR_t_sR_type

  let content_type = sR_t_sR_content_type

  let shared = sR_t_sR_shared

  let other_config = sR_t_sR_other_config

  let tags = sR_t_sR_tags

  let default_vdi_visibility = sR_t_sR_default_vdi_visibility

  let sm_config = sR_t_sR_sm_config

  let blobs = sR_t_sR_blobs

  let local_cache_enabled = sR_t_sR_local_cache_enabled

  let introduced_by = sR_t_sR_introduced_by

  let clustered = sR_t_sR_clustered

  let is_tools_sr = sR_t_sR_is_tools_sr
end

module SM = struct
  let uuid = sM_t_sM_uuid

  let name_label = sM_t_sM_name_label

  let name_description = sM_t_sM_name_description

  let _type = sM_t_sM_type

  let vendor = sM_t_sM_vendor

  let copyright = sM_t_sM_copyright

  let version = sM_t_sM_version

  let required_api_version = sM_t_sM_required_api_version

  let configuration = sM_t_sM_configuration

  let capabilities = sM_t_sM_capabilities

  let features = sM_t_sM_features

  let other_config = sM_t_sM_other_config

  let driver_filename = sM_t_sM_driver_filename

  let required_cluster_stack = sM_t_sM_required_cluster_stack
end

module VLAN = struct
  let uuid = vLAN_t_vLAN_uuid

  let tagged_PIF = vLAN_t_vLAN_tagged_PIF

  let untagged_PIF = vLAN_t_vLAN_untagged_PIF

  let tag = vLAN_t_vLAN_tag

  let other_config = vLAN_t_vLAN_other_config
end

module Bond = struct
  let uuid = bond_t_bond_uuid

  let master = bond_t_bond_master

  let slaves = bond_t_bond_slaves

  let other_config = bond_t_bond_other_config

  let primary_slave = bond_t_bond_primary_slave

  let mode = bond_t_bond_mode

  let properties = bond_t_bond_properties

  let links_up = bond_t_bond_links_up
end

module PIF_metrics = struct
  let uuid = pIF_metrics_t_pIF_metrics_uuid

  let io_read_kbs = pIF_metrics_t_pIF_metrics_io_read_kbs

  let io_write_kbs = pIF_metrics_t_pIF_metrics_io_write_kbs

  let carrier = pIF_metrics_t_pIF_metrics_carrier

  let vendor_id = pIF_metrics_t_pIF_metrics_vendor_id

  let vendor_name = pIF_metrics_t_pIF_metrics_vendor_name

  let device_id = pIF_metrics_t_pIF_metrics_device_id

  let device_name = pIF_metrics_t_pIF_metrics_device_name

  let speed = pIF_metrics_t_pIF_metrics_speed

  let duplex = pIF_metrics_t_pIF_metrics_duplex

  let pci_bus_path = pIF_metrics_t_pIF_metrics_pci_bus_path

  let last_updated = pIF_metrics_t_pIF_metrics_last_updated

  let other_config = pIF_metrics_t_pIF_metrics_other_config
end

module PIF = struct
  let uuid = pIF_t_pIF_uuid

  let device = pIF_t_pIF_device

  let network = pIF_t_pIF_network

  let host = pIF_t_pIF_host

  let mAC = pIF_t_pIF_MAC

  let mTU = pIF_t_pIF_MTU

  let vLAN = pIF_t_pIF_VLAN

  let device_name = pIF_t_pIF_device_name

  let metrics = pIF_t_pIF_metrics

  let physical = pIF_t_pIF_physical

  let currently_attached = pIF_t_pIF_currently_attached

  let ip_configuration_mode = pIF_t_pIF_ip_configuration_mode

  let iP = pIF_t_pIF_IP

  let netmask = pIF_t_pIF_netmask

  let gateway = pIF_t_pIF_gateway

  let dNS = pIF_t_pIF_DNS

  let bond_slave_of = pIF_t_pIF_bond_slave_of

  let bond_master_of = pIF_t_pIF_bond_master_of

  let vLAN_master_of = pIF_t_pIF_VLAN_master_of

  let vLAN_slave_of = pIF_t_pIF_VLAN_slave_of

  let management = pIF_t_pIF_management

  let other_config = pIF_t_pIF_other_config

  let disallow_unplug = pIF_t_pIF_disallow_unplug

  let tunnel_access_PIF_of = pIF_t_pIF_tunnel_access_PIF_of

  let tunnel_transport_PIF_of = pIF_t_pIF_tunnel_transport_PIF_of

  let ipv6_configuration_mode = pIF_t_pIF_ipv6_configuration_mode

  let iPv6 = pIF_t_pIF_IPv6

  let ipv6_gateway = pIF_t_pIF_ipv6_gateway

  let primary_address_type = pIF_t_pIF_primary_address_type

  let managed = pIF_t_pIF_managed

  let properties = pIF_t_pIF_properties

  let capabilities = pIF_t_pIF_capabilities

  let igmp_snooping_status = pIF_t_pIF_igmp_snooping_status

  let sriov_physical_PIF_of = pIF_t_pIF_sriov_physical_PIF_of

  let sriov_logical_PIF_of = pIF_t_pIF_sriov_logical_PIF_of

  let pCI = pIF_t_pIF_PCI
end

module VIF_metrics = struct
  let uuid = vIF_metrics_t_vIF_metrics_uuid

  let io_read_kbs = vIF_metrics_t_vIF_metrics_io_read_kbs

  let io_write_kbs = vIF_metrics_t_vIF_metrics_io_write_kbs

  let last_updated = vIF_metrics_t_vIF_metrics_last_updated

  let other_config = vIF_metrics_t_vIF_metrics_other_config
end

module VIF = struct
  let uuid = vIF_t_vIF_uuid

  let allowed_operations = vIF_t_vIF_allowed_operations

  let current_operations = vIF_t_vIF_current_operations

  let device = vIF_t_vIF_device

  let network = vIF_t_vIF_network

  let vM = vIF_t_vIF_VM

  let mAC = vIF_t_vIF_MAC

  let mTU = vIF_t_vIF_MTU

  let reserved = vIF_t_vIF_reserved

  let other_config = vIF_t_vIF_other_config

  let currently_attached = vIF_t_vIF_currently_attached

  let status_code = vIF_t_vIF_status_code

  let status_detail = vIF_t_vIF_status_detail

  let runtime_properties = vIF_t_vIF_runtime_properties

  let qos_algorithm_type = vIF_t_vIF_qos_algorithm_type

  let qos_algorithm_params = vIF_t_vIF_qos_algorithm_params

  let qos_supported_algorithms = vIF_t_vIF_qos_supported_algorithms

  let metrics = vIF_t_vIF_metrics

  let mAC_autogenerated = vIF_t_vIF_MAC_autogenerated

  let locking_mode = vIF_t_vIF_locking_mode

  let ipv4_allowed = vIF_t_vIF_ipv4_allowed

  let ipv6_allowed = vIF_t_vIF_ipv6_allowed

  let ipv4_configuration_mode = vIF_t_vIF_ipv4_configuration_mode

  let ipv4_addresses = vIF_t_vIF_ipv4_addresses

  let ipv4_gateway = vIF_t_vIF_ipv4_gateway

  let ipv6_configuration_mode = vIF_t_vIF_ipv6_configuration_mode

  let ipv6_addresses = vIF_t_vIF_ipv6_addresses

  let ipv6_gateway = vIF_t_vIF_ipv6_gateway

  let reserved_pci = vIF_t_vIF_reserved_pci
end

module Network = struct
  let uuid = network_t_network_uuid

  let name_label = network_t_network_name_label

  let name_description = network_t_network_name_description

  let allowed_operations = network_t_network_allowed_operations

  let current_operations = network_t_network_current_operations

  let vIFs = network_t_network_VIFs

  let pIFs = network_t_network_PIFs

  let mTU = network_t_network_MTU

  let other_config = network_t_network_other_config

  let bridge = network_t_network_bridge

  let managed = network_t_network_managed

  let blobs = network_t_network_blobs

  let tags = network_t_network_tags

  let default_locking_mode = network_t_network_default_locking_mode

  let assigned_ips = network_t_network_assigned_ips

  let purpose = network_t_network_purpose
end

module Host_cpu = struct
  let uuid = host_cpu_t_host_cpu_uuid

  let host = host_cpu_t_host_cpu_host

  let number = host_cpu_t_host_cpu_number

  let vendor = host_cpu_t_host_cpu_vendor

  let speed = host_cpu_t_host_cpu_speed

  let modelname = host_cpu_t_host_cpu_modelname

  let family = host_cpu_t_host_cpu_family

  let model = host_cpu_t_host_cpu_model

  let stepping = host_cpu_t_host_cpu_stepping

  let flags = host_cpu_t_host_cpu_flags

  let features = host_cpu_t_host_cpu_features

  let utilisation = host_cpu_t_host_cpu_utilisation

  let other_config = host_cpu_t_host_cpu_other_config
end

module Host_metrics = struct
  let uuid = host_metrics_t_host_metrics_uuid

  let memory_total = host_metrics_t_host_metrics_memory_total

  let memory_free = host_metrics_t_host_metrics_memory_free

  let live = host_metrics_t_host_metrics_live

  let last_updated = host_metrics_t_host_metrics_last_updated

  let other_config = host_metrics_t_host_metrics_other_config
end

module Host_patch = struct
  let uuid = host_patch_t_host_patch_uuid

  let name_label = host_patch_t_host_patch_name_label

  let name_description = host_patch_t_host_patch_name_description

  let version = host_patch_t_host_patch_version

  let host = host_patch_t_host_patch_host

  let filename = host_patch_t_host_patch_filename

  let applied = host_patch_t_host_patch_applied

  let timestamp_applied = host_patch_t_host_patch_timestamp_applied

  let size = host_patch_t_host_patch_size

  let pool_patch = host_patch_t_host_patch_pool_patch

  let other_config = host_patch_t_host_patch_other_config
end

module Host_crashdump = struct
  let uuid = host_crashdump_t_host_crashdump_uuid

  let host = host_crashdump_t_host_crashdump_host

  let timestamp = host_crashdump_t_host_crashdump_timestamp

  let size = host_crashdump_t_host_crashdump_size

  let filename = host_crashdump_t_host_crashdump_filename

  let other_config = host_crashdump_t_host_crashdump_other_config
end

module Host = struct
  let uuid = host_t_host_uuid

  let name_label = host_t_host_name_label

  let name_description = host_t_host_name_description

  let memory_overhead = host_t_host_memory_overhead

  let allowed_operations = host_t_host_allowed_operations

  let current_operations = host_t_host_current_operations

  let aPI_version_major = host_t_host_API_version_major

  let aPI_version_minor = host_t_host_API_version_minor

  let aPI_version_vendor = host_t_host_API_version_vendor

  let aPI_version_vendor_implementation =
    host_t_host_API_version_vendor_implementation

  let enabled = host_t_host_enabled

  let software_version = host_t_host_software_version

  let other_config = host_t_host_other_config

  let capabilities = host_t_host_capabilities

  let cpu_configuration = host_t_host_cpu_configuration

  let sched_policy = host_t_host_sched_policy

  let supported_bootloaders = host_t_host_supported_bootloaders

  let resident_VMs = host_t_host_resident_VMs

  let logging = host_t_host_logging

  let pIFs = host_t_host_PIFs

  let suspend_image_sr = host_t_host_suspend_image_sr

  let crash_dump_sr = host_t_host_crash_dump_sr

  let crashdumps = host_t_host_crashdumps

  let patches = host_t_host_patches

  let updates = host_t_host_updates

  let pBDs = host_t_host_PBDs

  let host_CPUs = host_t_host_host_CPUs

  let cpu_info = host_t_host_cpu_info

  let hostname = host_t_host_hostname

  let address = host_t_host_address

  let metrics = host_t_host_metrics

  let license_params = host_t_host_license_params

  let boot_free_mem = host_t_host_boot_free_mem

  let ha_statefiles = host_t_host_ha_statefiles

  let ha_network_peers = host_t_host_ha_network_peers

  let blobs = host_t_host_blobs

  let tags = host_t_host_tags

  let external_auth_type = host_t_host_external_auth_type

  let external_auth_service_name = host_t_host_external_auth_service_name

  let external_auth_configuration = host_t_host_external_auth_configuration

  let edition = host_t_host_edition

  let license_server = host_t_host_license_server

  let bios_strings = host_t_host_bios_strings

  let power_on_mode = host_t_host_power_on_mode

  let power_on_config = host_t_host_power_on_config

  let local_cache_sr = host_t_host_local_cache_sr

  let chipset_info = host_t_host_chipset_info

  let pCIs = host_t_host_PCIs

  let pGPUs = host_t_host_PGPUs

  let pUSBs = host_t_host_PUSBs

  let ssl_legacy = host_t_host_ssl_legacy

  let guest_VCPUs_params = host_t_host_guest_VCPUs_params

  let display = host_t_host_display

  let virtual_hardware_platform_versions =
    host_t_host_virtual_hardware_platform_versions

  let control_domain = host_t_host_control_domain

  let updates_requiring_reboot = host_t_host_updates_requiring_reboot

  let features = host_t_host_features

  let iscsi_iqn = host_t_host_iscsi_iqn

  let multipathing = host_t_host_multipathing
end

module DR_task = struct
  let uuid = dR_task_t_dR_task_uuid

  let introduced_SRs = dR_task_t_dR_task_introduced_SRs
end

module VM_appliance = struct
  let uuid = vM_appliance_t_vM_appliance_uuid

  let name_label = vM_appliance_t_vM_appliance_name_label

  let name_description = vM_appliance_t_vM_appliance_name_description

  let allowed_operations = vM_appliance_t_vM_appliance_allowed_operations

  let current_operations = vM_appliance_t_vM_appliance_current_operations

  let vMs = vM_appliance_t_vM_appliance_VMs
end

module VMSS = struct
  let uuid = vMSS_t_vMSS_uuid

  let name_label = vMSS_t_vMSS_name_label

  let name_description = vMSS_t_vMSS_name_description

  let enabled = vMSS_t_vMSS_enabled

  let _type = vMSS_t_vMSS_type

  let retained_snapshots = vMSS_t_vMSS_retained_snapshots

  let frequency = vMSS_t_vMSS_frequency

  let schedule = vMSS_t_vMSS_schedule

  let last_run_time = vMSS_t_vMSS_last_run_time

  let vMs = vMSS_t_vMSS_VMs
end

module VMPP = struct
  let uuid = vMPP_t_vMPP_uuid

  let name_label = vMPP_t_vMPP_name_label

  let name_description = vMPP_t_vMPP_name_description

  let is_policy_enabled = vMPP_t_vMPP_is_policy_enabled

  let backup_type = vMPP_t_vMPP_backup_type

  let backup_retention_value = vMPP_t_vMPP_backup_retention_value

  let backup_frequency = vMPP_t_vMPP_backup_frequency

  let backup_schedule = vMPP_t_vMPP_backup_schedule

  let is_backup_running = vMPP_t_vMPP_is_backup_running

  let backup_last_run_time = vMPP_t_vMPP_backup_last_run_time

  let archive_target_type = vMPP_t_vMPP_archive_target_type

  let archive_target_config = vMPP_t_vMPP_archive_target_config

  let archive_frequency = vMPP_t_vMPP_archive_frequency

  let archive_schedule = vMPP_t_vMPP_archive_schedule

  let is_archive_running = vMPP_t_vMPP_is_archive_running

  let archive_last_run_time = vMPP_t_vMPP_archive_last_run_time

  let vMs = vMPP_t_vMPP_VMs

  let is_alarm_enabled = vMPP_t_vMPP_is_alarm_enabled

  let alarm_config = vMPP_t_vMPP_alarm_config

  let recent_alerts = vMPP_t_vMPP_recent_alerts
end

module VM_guest_metrics = struct
  let uuid = vM_guest_metrics_t_vM_guest_metrics_uuid

  let os_version = vM_guest_metrics_t_vM_guest_metrics_os_version

  let pV_drivers_version =
    vM_guest_metrics_t_vM_guest_metrics_PV_drivers_version

  let pV_drivers_up_to_date =
    vM_guest_metrics_t_vM_guest_metrics_PV_drivers_up_to_date

  let memory = vM_guest_metrics_t_vM_guest_metrics_memory

  let disks = vM_guest_metrics_t_vM_guest_metrics_disks

  let networks = vM_guest_metrics_t_vM_guest_metrics_networks

  let other = vM_guest_metrics_t_vM_guest_metrics_other

  let last_updated = vM_guest_metrics_t_vM_guest_metrics_last_updated

  let other_config = vM_guest_metrics_t_vM_guest_metrics_other_config

  let live = vM_guest_metrics_t_vM_guest_metrics_live

  let can_use_hotplug_vbd =
    vM_guest_metrics_t_vM_guest_metrics_can_use_hotplug_vbd

  let can_use_hotplug_vif =
    vM_guest_metrics_t_vM_guest_metrics_can_use_hotplug_vif

  let pV_drivers_detected =
    vM_guest_metrics_t_vM_guest_metrics_PV_drivers_detected
end

module VM_metrics = struct
  let uuid = vM_metrics_t_vM_metrics_uuid

  let memory_actual = vM_metrics_t_vM_metrics_memory_actual

  let vCPUs_number = vM_metrics_t_vM_metrics_VCPUs_number

  let vCPUs_utilisation = vM_metrics_t_vM_metrics_VCPUs_utilisation

  let vCPUs_CPU = vM_metrics_t_vM_metrics_VCPUs_CPU

  let vCPUs_params = vM_metrics_t_vM_metrics_VCPUs_params

  let vCPUs_flags = vM_metrics_t_vM_metrics_VCPUs_flags

  let state = vM_metrics_t_vM_metrics_state

  let start_time = vM_metrics_t_vM_metrics_start_time

  let install_time = vM_metrics_t_vM_metrics_install_time

  let last_updated = vM_metrics_t_vM_metrics_last_updated

  let other_config = vM_metrics_t_vM_metrics_other_config

  let hvm = vM_metrics_t_vM_metrics_hvm

  let nested_virt = vM_metrics_t_vM_metrics_nested_virt

  let nomigrate = vM_metrics_t_vM_metrics_nomigrate

  let current_domain_type = vM_metrics_t_vM_metrics_current_domain_type
end

module VM = struct
  let uuid = vM_t_vM_uuid

  let allowed_operations = vM_t_vM_allowed_operations

  let current_operations = vM_t_vM_current_operations

  let power_state = vM_t_vM_power_state

  let name_label = vM_t_vM_name_label

  let name_description = vM_t_vM_name_description

  let user_version = vM_t_vM_user_version

  let is_a_template = vM_t_vM_is_a_template

  let is_default_template = vM_t_vM_is_default_template

  let suspend_VDI = vM_t_vM_suspend_VDI

  let resident_on = vM_t_vM_resident_on

  let scheduled_to_be_resident_on = vM_t_vM_scheduled_to_be_resident_on

  let affinity = vM_t_vM_affinity

  let memory_overhead = vM_t_vM_memory_overhead

  let memory_target = vM_t_vM_memory_target

  let memory_static_max = vM_t_vM_memory_static_max

  let memory_dynamic_max = vM_t_vM_memory_dynamic_max

  let memory_dynamic_min = vM_t_vM_memory_dynamic_min

  let memory_static_min = vM_t_vM_memory_static_min

  let vCPUs_params = vM_t_vM_VCPUs_params

  let vCPUs_max = vM_t_vM_VCPUs_max

  let vCPUs_at_startup = vM_t_vM_VCPUs_at_startup

  let actions_after_shutdown = vM_t_vM_actions_after_shutdown

  let actions_after_reboot = vM_t_vM_actions_after_reboot

  let actions_after_crash = vM_t_vM_actions_after_crash

  let consoles = vM_t_vM_consoles

  let vIFs = vM_t_vM_VIFs

  let vBDs = vM_t_vM_VBDs

  let vUSBs = vM_t_vM_VUSBs

  let crash_dumps = vM_t_vM_crash_dumps

  let vTPMs = vM_t_vM_VTPMs

  let pV_bootloader = vM_t_vM_PV_bootloader

  let pV_kernel = vM_t_vM_PV_kernel

  let pV_ramdisk = vM_t_vM_PV_ramdisk

  let pV_args = vM_t_vM_PV_args

  let pV_bootloader_args = vM_t_vM_PV_bootloader_args

  let pV_legacy_args = vM_t_vM_PV_legacy_args

  let hVM_boot_policy = vM_t_vM_HVM_boot_policy

  let hVM_boot_params = vM_t_vM_HVM_boot_params

  let hVM_shadow_multiplier = vM_t_vM_HVM_shadow_multiplier

  let platform = vM_t_vM_platform

  let pCI_bus = vM_t_vM_PCI_bus

  let other_config = vM_t_vM_other_config

  let domid = vM_t_vM_domid

  let domarch = vM_t_vM_domarch

  let last_boot_CPU_flags = vM_t_vM_last_boot_CPU_flags

  let is_control_domain = vM_t_vM_is_control_domain

  let metrics = vM_t_vM_metrics

  let guest_metrics = vM_t_vM_guest_metrics

  let last_booted_record = vM_t_vM_last_booted_record

  let recommendations = vM_t_vM_recommendations

  let xenstore_data = vM_t_vM_xenstore_data

  let ha_always_run = vM_t_vM_ha_always_run

  let ha_restart_priority = vM_t_vM_ha_restart_priority

  let is_a_snapshot = vM_t_vM_is_a_snapshot

  let snapshot_of = vM_t_vM_snapshot_of

  let snapshots = vM_t_vM_snapshots

  let snapshot_time = vM_t_vM_snapshot_time

  let transportable_snapshot_id = vM_t_vM_transportable_snapshot_id

  let blobs = vM_t_vM_blobs

  let tags = vM_t_vM_tags

  let blocked_operations = vM_t_vM_blocked_operations

  let snapshot_info = vM_t_vM_snapshot_info

  let snapshot_metadata = vM_t_vM_snapshot_metadata

  let parent = vM_t_vM_parent

  let children = vM_t_vM_children

  let bios_strings = vM_t_vM_bios_strings

  let protection_policy = vM_t_vM_protection_policy

  let is_snapshot_from_vmpp = vM_t_vM_is_snapshot_from_vmpp

  let snapshot_schedule = vM_t_vM_snapshot_schedule

  let is_vmss_snapshot = vM_t_vM_is_vmss_snapshot

  let appliance = vM_t_vM_appliance

  let start_delay = vM_t_vM_start_delay

  let shutdown_delay = vM_t_vM_shutdown_delay

  let order = vM_t_vM_order

  let vGPUs = vM_t_vM_VGPUs

  let attached_PCIs = vM_t_vM_attached_PCIs

  let suspend_SR = vM_t_vM_suspend_SR

  let version = vM_t_vM_version

  let generation_id = vM_t_vM_generation_id

  let hardware_platform_version = vM_t_vM_hardware_platform_version

  let has_vendor_device = vM_t_vM_has_vendor_device

  let requires_reboot = vM_t_vM_requires_reboot

  let reference_label = vM_t_vM_reference_label

  let domain_type = vM_t_vM_domain_type

  let nVRAM = vM_t_vM_NVRAM
end

module Pool_update = struct
  let uuid = pool_update_t_pool_update_uuid

  let name_label = pool_update_t_pool_update_name_label

  let name_description = pool_update_t_pool_update_name_description

  let version = pool_update_t_pool_update_version

  let installation_size = pool_update_t_pool_update_installation_size

  let key = pool_update_t_pool_update_key

  let after_apply_guidance = pool_update_t_pool_update_after_apply_guidance

  let vdi = pool_update_t_pool_update_vdi

  let hosts = pool_update_t_pool_update_hosts

  let other_config = pool_update_t_pool_update_other_config

  let enforce_homogeneity = pool_update_t_pool_update_enforce_homogeneity
end

module Pool_patch = struct
  let uuid = pool_patch_t_pool_patch_uuid

  let name_label = pool_patch_t_pool_patch_name_label

  let name_description = pool_patch_t_pool_patch_name_description

  let version = pool_patch_t_pool_patch_version

  let filename = pool_patch_t_pool_patch_filename

  let size = pool_patch_t_pool_patch_size

  let pool_applied = pool_patch_t_pool_patch_pool_applied

  let host_patches = pool_patch_t_pool_patch_host_patches

  let after_apply_guidance = pool_patch_t_pool_patch_after_apply_guidance

  let pool_update = pool_patch_t_pool_patch_pool_update

  let other_config = pool_patch_t_pool_patch_other_config
end

module Pool = struct
  let uuid = pool_t_pool_uuid

  let name_label = pool_t_pool_name_label

  let name_description = pool_t_pool_name_description

  let master = pool_t_pool_master

  let default_SR = pool_t_pool_default_SR

  let suspend_image_SR = pool_t_pool_suspend_image_SR

  let crash_dump_SR = pool_t_pool_crash_dump_SR

  let other_config = pool_t_pool_other_config

  let ha_enabled = pool_t_pool_ha_enabled

  let ha_configuration = pool_t_pool_ha_configuration

  let ha_statefiles = pool_t_pool_ha_statefiles

  let ha_host_failures_to_tolerate = pool_t_pool_ha_host_failures_to_tolerate

  let ha_plan_exists_for = pool_t_pool_ha_plan_exists_for

  let ha_allow_overcommit = pool_t_pool_ha_allow_overcommit

  let ha_overcommitted = pool_t_pool_ha_overcommitted

  let blobs = pool_t_pool_blobs

  let tags = pool_t_pool_tags

  let gui_config = pool_t_pool_gui_config

  let health_check_config = pool_t_pool_health_check_config

  let wlb_url = pool_t_pool_wlb_url

  let wlb_username = pool_t_pool_wlb_username

  let wlb_password = pool_t_pool_wlb_password

  let wlb_enabled = pool_t_pool_wlb_enabled

  let wlb_verify_cert = pool_t_pool_wlb_verify_cert

  let redo_log_enabled = pool_t_pool_redo_log_enabled

  let redo_log_vdi = pool_t_pool_redo_log_vdi

  let vswitch_controller = pool_t_pool_vswitch_controller

  let restrictions = pool_t_pool_restrictions

  let metadata_VDIs = pool_t_pool_metadata_VDIs

  let ha_cluster_stack = pool_t_pool_ha_cluster_stack

  let allowed_operations = pool_t_pool_allowed_operations

  let current_operations = pool_t_pool_current_operations

  let guest_agent_config = pool_t_pool_guest_agent_config

  let cpu_info = pool_t_pool_cpu_info

  let policy_no_vendor_device = pool_t_pool_policy_no_vendor_device

  let live_patching_disabled = pool_t_pool_live_patching_disabled

  let igmp_snooping_enabled = pool_t_pool_igmp_snooping_enabled
end

module Task = struct
  let uuid = task_t_task_uuid

  let name_label = task_t_task_name_label

  let name_description = task_t_task_name_description

  let allowed_operations = task_t_task_allowed_operations

  let current_operations = task_t_task_current_operations

  let created = task_t_task_created

  let finished = task_t_task_finished

  let status = task_t_task_status

  let session = task_t_task_session

  let resident_on = task_t_task_resident_on

  let progress = task_t_task_progress

  let externalpid = task_t_task_externalpid

  let stunnelpid = task_t_task_stunnelpid

  let forwarded = task_t_task_forwarded

  let forwarded_to = task_t_task_forwarded_to

  let _type = task_t_task_type

  let result = task_t_task_result

  let error_info = task_t_task_error_info

  let other_config = task_t_task_other_config

  let subtask_of = task_t_task_subtask_of

  let subtasks = task_t_task_subtasks

  let backtrace = task_t_task_backtrace
end

module Role = struct
  let uuid = role_t_role_uuid

  let name_label = role_t_role_name_label

  let name_description = role_t_role_name_description

  let subroles = role_t_role_subroles
end

module Subject = struct
  let uuid = subject_t_subject_uuid

  let subject_identifier = subject_t_subject_subject_identifier

  let other_config = subject_t_subject_other_config

  let roles = subject_t_subject_roles
end

module Session = struct
  let uuid = session_t_session_uuid

  let this_host = session_t_session_this_host

  let this_user = session_t_session_this_user

  let last_active = session_t_session_last_active

  let pool = session_t_session_pool

  let other_config = session_t_session_other_config

  let is_local_superuser = session_t_session_is_local_superuser

  let subject = session_t_session_subject

  let validation_time = session_t_session_validation_time

  let auth_user_sid = session_t_session_auth_user_sid

  let auth_user_name = session_t_session_auth_user_name

  let rbac_permissions = session_t_session_rbac_permissions

  let tasks = session_t_session_tasks

  let parent = session_t_session_parent

  let originator = session_t_session_originator
end

module Event = struct
  let id = event_t_event_id

  let timestamp = event_t_event_timestamp

  let _class = event_t_event_class

  let operation = event_t_event_operation

  let _ref = event_t_event_ref

  let obj_uuid = event_t_event_obj_uuid
end

module Data_source = struct
  let name_label = data_source_t_data_source_name_label

  let name_description = data_source_t_data_source_name_description

  let enabled = data_source_t_data_source_enabled

  let standard = data_source_t_data_source_standard

  let units = data_source_t_data_source_units

  let min = data_source_t_data_source_min

  let max = data_source_t_data_source_max

  let value = data_source_t_data_source_value
end

module Probe_result = struct
  let configuration = probe_result_t_probe_result_configuration

  let complete = probe_result_t_probe_result_complete

  let sr = probe_result_t_probe_result_sr

  let extra_info = probe_result_t_probe_result_extra_info
end

module Vdi_nbd_server_info = struct
  let exportname = vdi_nbd_server_info_t_vdi_nbd_server_info_exportname

  let address = vdi_nbd_server_info_t_vdi_nbd_server_info_address

  let port = vdi_nbd_server_info_t_vdi_nbd_server_info_port

  let cert = vdi_nbd_server_info_t_vdi_nbd_server_info_cert

  let subject = vdi_nbd_server_info_t_vdi_nbd_server_info_subject
end

module Message = struct
  let uuid = message_t_message_uuid

  let name = message_t_message_name

  let priority = message_t_message_priority

  let cls = message_t_message_cls

  let obj_uuid = message_t_message_obj_uuid

  let timestamp = message_t_message_timestamp

  let body = message_t_message_body
end

module Sr_stat = struct
  let uuid = sr_stat_t_sr_stat_uuid

  let name_label = sr_stat_t_sr_stat_name_label

  let name_description = sr_stat_t_sr_stat_name_description

  let free_space = sr_stat_t_sr_stat_free_space

  let total_space = sr_stat_t_sr_stat_total_space

  let clustered = sr_stat_t_sr_stat_clustered

  let health = sr_stat_t_sr_stat_health
end

type database =
  { sr_stat: sr_stat_t Rpc.Refmap.t
  ; message: message_t Rpc.Refmap.t
  ; vdi_nbd_server_info: vdi_nbd_server_info_t Rpc.Refmap.t
  ; probe_result: probe_result_t Rpc.Refmap.t
  ; data_source: data_source_t Rpc.Refmap.t
  ; event: event_t Rpc.Refmap.t
  ; session: session_t Rpc.Refmap.t
  ; subject: subject_t Rpc.Refmap.t
  ; role: role_t Rpc.Refmap.t
  ; task: task_t Rpc.Refmap.t
  ; pool: pool_t Rpc.Refmap.t
  ; pool_patch: pool_patch_t Rpc.Refmap.t
  ; pool_update: pool_update_t Rpc.Refmap.t
  ; vM: vM_t Rpc.Refmap.t
  ; vM_metrics: vM_metrics_t Rpc.Refmap.t
  ; vM_guest_metrics: vM_guest_metrics_t Rpc.Refmap.t
  ; vMPP: vMPP_t Rpc.Refmap.t
  ; vMSS: vMSS_t Rpc.Refmap.t
  ; vM_appliance: vM_appliance_t Rpc.Refmap.t
  ; dR_task: dR_task_t Rpc.Refmap.t
  ; host: host_t Rpc.Refmap.t
  ; host_crashdump: host_crashdump_t Rpc.Refmap.t
  ; host_patch: host_patch_t Rpc.Refmap.t
  ; host_metrics: host_metrics_t Rpc.Refmap.t
  ; host_cpu: host_cpu_t Rpc.Refmap.t
  ; network: network_t Rpc.Refmap.t
  ; vIF: vIF_t Rpc.Refmap.t
  ; vIF_metrics: vIF_metrics_t Rpc.Refmap.t
  ; pIF: pIF_t Rpc.Refmap.t
  ; pIF_metrics: pIF_metrics_t Rpc.Refmap.t
  ; bond: bond_t Rpc.Refmap.t
  ; vLAN: vLAN_t Rpc.Refmap.t
  ; sM: sM_t Rpc.Refmap.t
  ; sR: sR_t Rpc.Refmap.t
  ; lVHD: lVHD_t Rpc.Refmap.t
  ; vDI: vDI_t Rpc.Refmap.t
  ; vBD: vBD_t Rpc.Refmap.t
  ; vBD_metrics: vBD_metrics_t Rpc.Refmap.t
  ; pBD: pBD_t Rpc.Refmap.t
  ; crashdump: crashdump_t Rpc.Refmap.t
  ; vTPM: vTPM_t Rpc.Refmap.t
  ; console: console_t Rpc.Refmap.t
  ; user: user_t Rpc.Refmap.t
  ; blob: blob_t Rpc.Refmap.t
  ; secret: secret_t Rpc.Refmap.t
  ; tunnel: tunnel_t Rpc.Refmap.t
  ; network_sriov: network_sriov_t Rpc.Refmap.t
  ; pCI: pCI_t Rpc.Refmap.t
  ; pGPU: pGPU_t Rpc.Refmap.t
  ; gPU_group: gPU_group_t Rpc.Refmap.t
  ; vGPU: vGPU_t Rpc.Refmap.t
  ; vGPU_type: vGPU_type_t Rpc.Refmap.t
  ; pVS_site: pVS_site_t Rpc.Refmap.t
  ; pVS_server: pVS_server_t Rpc.Refmap.t
  ; pVS_proxy: pVS_proxy_t Rpc.Refmap.t
  ; pVS_cache_storage: pVS_cache_storage_t Rpc.Refmap.t
  ; feature: feature_t Rpc.Refmap.t
  ; sDN_controller: sDN_controller_t Rpc.Refmap.t
  ; pUSB: pUSB_t Rpc.Refmap.t
  ; uSB_group: uSB_group_t Rpc.Refmap.t
  ; vUSB: vUSB_t Rpc.Refmap.t
  ; cluster: cluster_t Rpc.Refmap.t
  ; cluster_host: cluster_host_t Rpc.Refmap.t }
[@@deriving rpcty]

let empty_db =
  { sr_stat= Refmap.empty
  ; message= Refmap.empty
  ; vdi_nbd_server_info= Refmap.empty
  ; probe_result= Refmap.empty
  ; data_source= Refmap.empty
  ; event= Refmap.empty
  ; session= Refmap.empty
  ; subject= Refmap.empty
  ; role= Refmap.empty
  ; task= Refmap.empty
  ; pool= Refmap.empty
  ; pool_patch= Refmap.empty
  ; pool_update= Refmap.empty
  ; vM= Refmap.empty
  ; vM_metrics= Refmap.empty
  ; vM_guest_metrics= Refmap.empty
  ; vMPP= Refmap.empty
  ; vMSS= Refmap.empty
  ; vM_appliance= Refmap.empty
  ; dR_task= Refmap.empty
  ; host= Refmap.empty
  ; host_crashdump= Refmap.empty
  ; host_patch= Refmap.empty
  ; host_metrics= Refmap.empty
  ; host_cpu= Refmap.empty
  ; network= Refmap.empty
  ; vIF= Refmap.empty
  ; vIF_metrics= Refmap.empty
  ; pIF= Refmap.empty
  ; pIF_metrics= Refmap.empty
  ; bond= Refmap.empty
  ; vLAN= Refmap.empty
  ; sM= Refmap.empty
  ; sR= Refmap.empty
  ; lVHD= Refmap.empty
  ; vDI= Refmap.empty
  ; vBD= Refmap.empty
  ; vBD_metrics= Refmap.empty
  ; pBD= Refmap.empty
  ; crashdump= Refmap.empty
  ; vTPM= Refmap.empty
  ; console= Refmap.empty
  ; user= Refmap.empty
  ; blob= Refmap.empty
  ; secret= Refmap.empty
  ; tunnel= Refmap.empty
  ; network_sriov= Refmap.empty
  ; pCI= Refmap.empty
  ; pGPU= Refmap.empty
  ; gPU_group= Refmap.empty
  ; vGPU= Refmap.empty
  ; vGPU_type= Refmap.empty
  ; pVS_site= Refmap.empty
  ; pVS_server= Refmap.empty
  ; pVS_proxy= Refmap.empty
  ; pVS_cache_storage= Refmap.empty
  ; feature= Refmap.empty
  ; sDN_controller= Refmap.empty
  ; pUSB= Refmap.empty
  ; uSB_group= Refmap.empty
  ; vUSB= Refmap.empty
  ; cluster= Refmap.empty
  ; cluster_host= Refmap.empty }

let rels =
  Dbgen.
    [ OtmRel (VM.snapshot_of, VM.snapshots)
    ; OtmRel (VDI.snapshot_of, VDI.snapshots)
    ; OtmRel (VM.parent, VM.children)
    ; OtmRel (Task.subtask_of, Task.subtasks)
    ; OtmRel (Task.session, Session.tasks)
    ; OtmRel (PIF.bond_slave_of, Bond.slaves)
    ; OtmRel (Bond.master, PIF.bond_master_of)
    ; OtmRel (VLAN.tagged_PIF, PIF.vLAN_slave_of)
    ; OtmRel (Tunnel.access_PIF, PIF.tunnel_access_PIF_of)
    ; OtmRel (Tunnel.transport_PIF, PIF.tunnel_transport_PIF_of)
    ; OtmRel (PBD.host, Host.pBDs)
    ; OtmRel (PBD.sR, SR.pBDs)
    ; OtmRel (VBD.vDI, VDI.vBDs)
    ; OtmRel (Crashdump.vDI, VDI.crash_dumps)
    ; OtmRel (VBD.vM, VM.vBDs)
    ; OtmRel (Crashdump.vM, VM.crash_dumps)
    ; OtmRel (VIF.vM, VM.vIFs)
    ; OtmRel (VIF.network, Network.vIFs)
    ; OtmRel (Cluster_host.cluster, Cluster.cluster_hosts)
    ; OtmRel (PIF.host, Host.pIFs)
    ; OtmRel (PIF.network, Network.pIFs)
    ; OtmRel (VDI.sR, SR.vDIs)
    ; OtmRel (VTPM.vM, VM.vTPMs)
    ; OtmRel (Console.vM, VM.consoles)
    ; OtmRel (VM.resident_on, Host.resident_VMs)
    ; OtmRel (Host_cpu.host, Host.host_CPUs)
    ; OtmRel (Host_crashdump.host, Host.crashdumps)
    ; OtmRel (Host_patch.host, Host.patches)
    ; OtmRel (Host_patch.pool_patch, Pool_patch.host_patches)
    ; MtmRel (Host.updates, Pool_update.hosts)
    ; (*MtmRel (Subject.roles, Subject.roles);*)
      MtmRel (Role.subroles, Role.subroles)
    ; OtmRel (VM.protection_policy, VMPP.vMs)
    ; OtmRel (VM.snapshot_schedule, VMSS.vMs)
    ; OtmRel (VM.appliance, VM_appliance.vMs)
    ; OtmRel (PGPU.gPU_group, GPU_group.pGPUs)
    ; OtmRel (VGPU.gPU_group, GPU_group.vGPUs)
    ; OtmRel (VGPU._type, VGPU_type.vGPUs)
    ; OtmRel (VGPU.vM, VM.vGPUs)
    ; OtmRel (VGPU.resident_on, PGPU.resident_VGPUs)
    ; MtmRel (PGPU.supported_VGPU_types, VGPU_type.supported_on_PGPUs)
    ; MtmRel (PGPU.enabled_VGPU_types, VGPU_type.enabled_on_PGPUs)
    ; MtmRel (GPU_group.supported_VGPU_types, VGPU_type.supported_on_GPU_groups)
    ; MtmRel (GPU_group.enabled_VGPU_types, VGPU_type.enabled_on_GPU_groups)
    ; OtmRel (PCI.host, Host.pCIs)
    ; OtmRel (PGPU.host, Host.pGPUs)
    ; MtmRel (PCI.attached_VMs, VM.attached_PCIs)
    ; OtmRel (PCI.physical_function, PCI.virtual_functions)
    ; OtmRel (VDI.metadata_of_pool, Pool.metadata_VDIs)
    ; OtmRel (SR.introduced_by, DR_task.introduced_SRs)
    ; OtmRel (PVS_server.site, PVS_site.servers)
    ; OtmRel (PVS_proxy.site, PVS_site.proxies)
    ; OtmRel (PVS_cache_storage.site, PVS_site.cache_storage)
    ; OtmRel (PUSB.host, Host.pUSBs)
    ; OtmRel (PUSB.uSB_group, USB_group.pUSBs)
    ; OtmRel (VUSB.uSB_group, USB_group.vUSBs)
    ; OtmRel (VUSB.vM, VM.vUSBs)
    ; OtmRel (Feature.host, Host.features)
    ; OtmRel (Network_sriov.physical_PIF, PIF.sriov_physical_PIF_of)
    ; OtmRel (Network_sriov.logical_PIF, PIF.sriov_logical_PIF_of) ]

let find_objs : type a.
    a Rpc.Types.cls -> (a Rpc.Refmap.t, database) Rpc.Types.field = function
  | SR_STAT_T -> database_sr_stat
  | MESSAGE_T -> database_message
  | VDI_NBD_SERVER_INFO_T -> database_vdi_nbd_server_info
  | PROBE_RESULT_T -> database_probe_result
  | DATA_SOURCE_T -> database_data_source
  | EVENT_T -> database_event
  | SESSION_T -> database_session
  | SUBJECT_T -> database_subject
  | ROLE_T -> database_role
  | TASK_T -> database_task
  | POOL_T -> database_pool
  | POOL_PATCH_T -> database_pool_patch
  | POOL_UPDATE_T -> database_pool_update
  | VM_T -> database_vM
  | VM_METRICS_T -> database_vM_metrics
  | VM_GUEST_METRICS_T -> database_vM_guest_metrics
  | VMPP_T -> database_vMPP
  | VMSS_T -> database_vMSS
  | VM_APPLIANCE_T -> database_vM_appliance
  | DR_TASK_T -> database_dR_task
  | HOST_T -> database_host
  | HOST_CRASHDUMP_T -> database_host_crashdump
  | HOST_PATCH_T -> database_host_patch
  | HOST_METRICS_T -> database_host_metrics
  | HOST_CPU_T -> database_host_cpu
  | NETWORK_T -> database_network
  | VIF_T -> database_vIF
  | VIF_METRICS_T -> database_vIF_metrics
  | PIF_T -> database_pIF
  | PIF_METRICS_T -> database_pIF_metrics
  | BOND_T -> database_bond
  | VLAN_T -> database_vLAN
  | SM_T -> database_sM
  | SR_T -> database_sR
  | LVHD_T -> database_lVHD
  | VDI_T -> database_vDI
  | VBD_T -> database_vBD
  | VBD_METRICS_T -> database_vBD_metrics
  | PBD_T -> database_pBD
  | CRASHDUMP_T -> database_crashdump
  | VTPM_T -> database_vTPM
  | CONSOLE_T -> database_console
  | USER_T -> database_user
  | BLOB_T -> database_blob
  | SECRET_T -> database_secret
  | TUNNEL_T -> database_tunnel
  | NETWORK_SRIOV_T -> database_network_sriov
  | PCI_T -> database_pCI
  | PGPU_T -> database_pGPU
  | GPU_GROUP_T -> database_gPU_group
  | VGPU_T -> database_vGPU
  | VGPU_TYPE_T -> database_vGPU_type
  | PVS_SITE_T -> database_pVS_site
  | PVS_SERVER_T -> database_pVS_server
  | PVS_PROXY_T -> database_pVS_proxy
  | PVS_CACHE_STORAGE_T -> database_pVS_cache_storage
  | FEATURE_T -> database_feature
  | SDN_CONTROLLER_T -> database_sDN_controller
  | PUSB_T -> database_pUSB
  | USB_GROUP_T -> database_uSB_group
  | VUSB_T -> database_vUSB
  | CLUSTER_T -> database_cluster
  | CLUSTER_HOST_T -> database_cluster_host
  | _ -> failwith "Invalid type. Did you remember to add it to API.find_objs?"

let typ_of_cls : type a. a Rpc.Types.cls -> a Rpc.Types.typ =
 fun cls ->
  match cls with
  | SR_STAT_T -> typ_of_sr_stat_t
  | MESSAGE_T -> typ_of_message_t
  | VDI_NBD_SERVER_INFO_T -> typ_of_vdi_nbd_server_info_t
  | PROBE_RESULT_T -> typ_of_probe_result_t
  | DATA_SOURCE_T -> typ_of_data_source_t
  | EVENT_T -> typ_of_event_t
  | SESSION_T -> typ_of_session_t
  | SUBJECT_T -> typ_of_subject_t
  | ROLE_T -> typ_of_role_t
  | TASK_T -> typ_of_task_t
  | POOL_T -> typ_of_pool_t
  | POOL_PATCH_T -> typ_of_pool_patch_t
  | POOL_UPDATE_T -> typ_of_pool_update_t
  | VM_T -> typ_of_vM_t
  | VM_METRICS_T -> typ_of_vM_metrics_t
  | VM_GUEST_METRICS_T -> typ_of_vM_guest_metrics_t
  | VMPP_T -> typ_of_vMPP_t
  | VMSS_T -> typ_of_vMSS_t
  | VM_APPLIANCE_T -> typ_of_vM_appliance_t
  | DR_TASK_T -> typ_of_dR_task_t
  | HOST_T -> typ_of_host_t
  | HOST_CRASHDUMP_T -> typ_of_host_crashdump_t
  | HOST_PATCH_T -> typ_of_host_patch_t
  | HOST_METRICS_T -> typ_of_host_metrics_t
  | HOST_CPU_T -> typ_of_host_cpu_t
  | NETWORK_T -> typ_of_network_t
  | VIF_T -> typ_of_vIF_t
  | VIF_METRICS_T -> typ_of_vIF_metrics_t
  | PIF_T -> typ_of_pIF_t
  | PIF_METRICS_T -> typ_of_pIF_metrics_t
  | BOND_T -> typ_of_bond_t
  | VLAN_T -> typ_of_vLAN_t
  | SM_T -> typ_of_sM_t
  | SR_T -> typ_of_sR_t
  | LVHD_T -> typ_of_lVHD_t
  | VDI_T -> typ_of_vDI_t
  | VBD_T -> typ_of_vBD_t
  | VBD_METRICS_T -> typ_of_vBD_metrics_t
  | PBD_T -> typ_of_pBD_t
  | CRASHDUMP_T -> typ_of_crashdump_t
  | VTPM_T -> typ_of_vTPM_t
  | CONSOLE_T -> typ_of_console_t
  | USER_T -> typ_of_user_t
  | BLOB_T -> typ_of_blob_t
  | SECRET_T -> typ_of_secret_t
  | TUNNEL_T -> typ_of_tunnel_t
  | NETWORK_SRIOV_T -> typ_of_network_sriov_t
  | PCI_T -> typ_of_pCI_t
  | PGPU_T -> typ_of_pGPU_t
  | GPU_GROUP_T -> typ_of_gPU_group_t
  | VGPU_T -> typ_of_vGPU_t
  | VGPU_TYPE_T -> typ_of_vGPU_type_t
  | PVS_SITE_T -> typ_of_pVS_site_t
  | PVS_SERVER_T -> typ_of_pVS_server_t
  | PVS_PROXY_T -> typ_of_pVS_proxy_t
  | PVS_CACHE_STORAGE_T -> typ_of_pVS_cache_storage_t
  | FEATURE_T -> typ_of_feature_t
  | SDN_CONTROLLER_T -> typ_of_sDN_controller_t
  | PUSB_T -> typ_of_pUSB_t
  | USB_GROUP_T -> typ_of_uSB_group_t
  | VUSB_T -> typ_of_vUSB_t
  | CLUSTER_T -> typ_of_cluster_t
  | CLUSTER_HOST_T -> typ_of_cluster_host_t
  | _ -> failwith "Unknown typ for cls"

let mkref cls =
  let typ = typ_of_cls cls in
  Rpc.Types.make_ref cls typ
