#' Title
#'
#' @param input_file_path file path where costar xlsx files are kept
#' @param output_file output file path and filename
#'
#' @return reads in data, cleans and saves to rds file
#' @export
#'
#' @examples
costar_property <- function(input_file_path, output_file){
  file_list <- list.files(path =  input_file_path,
                          pattern = "xlsx",
                          full.names = T)

  raw_dat <- purrr::map_df(.x = file_list,
                           .f = bearing::read_big_file)


  dat1 <- raw_dat %>%
    dplyr::select(1:217) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      last_sale_date = lubridate::ymd(last_sale_date),
      star_rating = haven::as_factor(star_rating),
      # STAR RATING BEST 5 TO 1 / Building Class A to Z
      dplyr::across(
        .fns = haven::as_factor,
        .cols = c(
          market_name,
          submarket_name,
          submarket_cluster,
          building_class,
          building_status,
          zip,
          construction_material
        )
      ),
      dplyr::across(.fns = as.logical, .cols = c(sprinklers)),
      dplyr::across(.fns = as.character, .cols = contains(c(
        "_phone", "_fax", "property_id"
      ))),
      dplyr::across(.fns = as.numeric, .cols = average_weighted_rent)
    )

  # mutate - create new columns
  dat2 <- dat1 %>%
    dplyr::mutate(ltb_ratio = land_area_sf / rba,
                  far = rba / land_area_sf)


  # Relocate Columns

  ### REorder Cols - First set is move to the front ## second set is move to the back
  dat3 <-  dat2 %>% dplyr::relocate(
    .before = NULL,
    dplyr::contains("parcel"),
    c(
      property_address,
      property_name,
      property_type,
      secondary_type,
      tenancy
    ),
    c(city, state, zip, county_name),
    c(rba, total_available_space_sf, land_area_ac, land_area_sf),
    c(ltb_ratio, far),
    #mutate create cols
    c(year_built, year_renovated),
    c(market_name, submarket_name, submarket_cluster, building_park),
    c(star_rating, building_class, building_status)
  ) %>%
    dplyr::relocate(
      .after = dplyr::last_col(),
      c(parking_ratio,  number_of_parking_spaces),
      dplyr::contains(
        c(
          "zoning",
          "construction_material",
          "elevator",
          "sprinklers",
          "amenities",
          "stories",
          "property_location",
          "cross_street",
          "features",
          "drive_ins"
        )
      ),
      c(ceiling_height_range, power),
      dplyr::contains(c("direct_services", "services", "rent")),
      dplyr::contains(c("vacancy", "occupancy", "percent_leased")),
      c(
        "anchor_gla",
        "core_factor",
        "office_space",
        "typical_floor_size",
        "max_building_contiguous_space",
        "max_floor_contiguous_space",
        "smallest_available_space"
      ),
      c(
        "direct_available_space",
        "direct_vacant_space",
        "sublet_available_space",
        "sublet_vacant_space",
        "total_sublet_space_sf",
        "total_relet_space_sf",
        "total_vacant_avail_relet_space_sf",
        "total_vacant_avail_sublet_space_sf",
        "total_vacant_available"
      ),
      dplyr::contains(c(
        "ops_expense", "operating_expenses", "exp_year"
      )),
      dplyr::contains("tax"),
      dplyr::contains(c("energy_star", "leed_")),
      dplyr::contains(c("closest_transit_stop")),
      dplyr::contains(
        c(
          "studio",
          "1_bed",
          "one_bedroom",
          "2_bed",
          "two_bedroom",
          "3_bed",
          "three_bedroom",
          "4_bed",
          "four_bedroom"
        )
      ),
      #Apartment
      c(
        number_of_beds,
        number_of_rooms,
        number_of_cranes,
        number_of_loading_docks
      ),
      c(
        avg_asking_sf,
        avg_effective_sf,
        avg_asking_unit,
        avg_effective_unit,
        avg_asking_bed,
        avg_concessions_percent
      ),
      c(price_unit, avg_unit_sf),

      c(power, gas, heating, water, sewer, affordable_type, university),
      c(anchor_tenants, architect_name),
      dplyr::contains(c("number")),
      c(for_sale_price, for_sale_status, days_on_market),
      dplyr::contains(c("last_sale", "cap_rate", "sales_")),
      dplyr::contains(c("longitude", "latitude")),
      property_id,
      dplyr::contains(
        c(
          "owner_",
          "true_owner",
          "recorded_owner",
          "sale_company",
          "property_manager",
          "leasing_company",
          "primary_agent"
        )
      )
    )

  dat <- janitor::remove_empty(dat3, which = "cols")

  saveRDS(dat,output_file)

}
