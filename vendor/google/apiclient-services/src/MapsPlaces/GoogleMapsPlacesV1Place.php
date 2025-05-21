<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\MapsPlaces;

class GoogleMapsPlacesV1Place extends \Google\Collection
{
  protected $collection_key = 'types';
  protected $accessibilityOptionsType = GoogleMapsPlacesV1PlaceAccessibilityOptions::class;
  protected $accessibilityOptionsDataType = '';
  protected $addressComponentsType = GoogleMapsPlacesV1PlaceAddressComponent::class;
  protected $addressComponentsDataType = 'array';
  protected $addressDescriptorType = GoogleMapsPlacesV1AddressDescriptor::class;
  protected $addressDescriptorDataType = '';
  /**
   * @var string
   */
  public $adrFormatAddress;
  /**
   * @var bool
   */
  public $allowsDogs;
  protected $areaSummaryType = GoogleMapsPlacesV1PlaceAreaSummary::class;
  protected $areaSummaryDataType = '';
  protected $attributionsType = GoogleMapsPlacesV1PlaceAttribution::class;
  protected $attributionsDataType = 'array';
  /**
   * @var string
   */
  public $businessStatus;
  protected $containingPlacesType = GoogleMapsPlacesV1PlaceContainingPlace::class;
  protected $containingPlacesDataType = 'array';
  /**
   * @var bool
   */
  public $curbsidePickup;
  protected $currentOpeningHoursType = GoogleMapsPlacesV1PlaceOpeningHours::class;
  protected $currentOpeningHoursDataType = '';
  protected $currentSecondaryOpeningHoursType = GoogleMapsPlacesV1PlaceOpeningHours::class;
  protected $currentSecondaryOpeningHoursDataType = 'array';
  /**
   * @var bool
   */
  public $delivery;
  /**
   * @var bool
   */
  public $dineIn;
  protected $displayNameType = GoogleTypeLocalizedText::class;
  protected $displayNameDataType = '';
  protected $editorialSummaryType = GoogleTypeLocalizedText::class;
  protected $editorialSummaryDataType = '';
  protected $evChargeOptionsType = GoogleMapsPlacesV1EVChargeOptions::class;
  protected $evChargeOptionsDataType = '';
  /**
   * @var string
   */
  public $formattedAddress;
  protected $fuelOptionsType = GoogleMapsPlacesV1FuelOptions::class;
  protected $fuelOptionsDataType = '';
  protected $generativeSummaryType = GoogleMapsPlacesV1PlaceGenerativeSummary::class;
  protected $generativeSummaryDataType = '';
  /**
   * @var bool
   */
  public $goodForChildren;
  /**
   * @var bool
   */
  public $goodForGroups;
  /**
   * @var bool
   */
  public $goodForWatchingSports;
  protected $googleMapsLinksType = GoogleMapsPlacesV1PlaceGoogleMapsLinks::class;
  protected $googleMapsLinksDataType = '';
  /**
   * @var string
   */
  public $googleMapsUri;
  /**
   * @var string
   */
  public $iconBackgroundColor;
  /**
   * @var string
   */
  public $iconMaskBaseUri;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $internationalPhoneNumber;
  /**
   * @var bool
   */
  public $liveMusic;
  protected $locationType = GoogleTypeLatLng::class;
  protected $locationDataType = '';
  /**
   * @var bool
   */
  public $menuForChildren;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $nationalPhoneNumber;
  /**
   * @var bool
   */
  public $outdoorSeating;
  protected $parkingOptionsType = GoogleMapsPlacesV1PlaceParkingOptions::class;
  protected $parkingOptionsDataType = '';
  protected $paymentOptionsType = GoogleMapsPlacesV1PlacePaymentOptions::class;
  protected $paymentOptionsDataType = '';
  protected $photosType = GoogleMapsPlacesV1Photo::class;
  protected $photosDataType = 'array';
  protected $plusCodeType = GoogleMapsPlacesV1PlacePlusCode::class;
  protected $plusCodeDataType = '';
  /**
   * @var string
   */
  public $priceLevel;
  protected $priceRangeType = GoogleMapsPlacesV1PriceRange::class;
  protected $priceRangeDataType = '';
  /**
   * @var string
   */
  public $primaryType;
  protected $primaryTypeDisplayNameType = GoogleTypeLocalizedText::class;
  protected $primaryTypeDisplayNameDataType = '';
  /**
   * @var bool
   */
  public $pureServiceAreaBusiness;
  public $rating;
  protected $regularOpeningHoursType = GoogleMapsPlacesV1PlaceOpeningHours::class;
  protected $regularOpeningHoursDataType = '';
  protected $regularSecondaryOpeningHoursType = GoogleMapsPlacesV1PlaceOpeningHours::class;
  protected $regularSecondaryOpeningHoursDataType = 'array';
  /**
   * @var bool
   */
  public $reservable;
  /**
   * @var bool
   */
  public $restroom;
  protected $reviewsType = GoogleMapsPlacesV1Review::class;
  protected $reviewsDataType = 'array';
  /**
   * @var bool
   */
  public $servesBeer;
  /**
   * @var bool
   */
  public $servesBreakfast;
  /**
   * @var bool
   */
  public $servesBrunch;
  /**
   * @var bool
   */
  public $servesCocktails;
  /**
   * @var bool
   */
  public $servesCoffee;
  /**
   * @var bool
   */
  public $servesDessert;
  /**
   * @var bool
   */
  public $servesDinner;
  /**
   * @var bool
   */
  public $servesLunch;
  /**
   * @var bool
   */
  public $servesVegetarianFood;
  /**
   * @var bool
   */
  public $servesWine;
  /**
   * @var string
   */
  public $shortFormattedAddress;
  protected $subDestinationsType = GoogleMapsPlacesV1PlaceSubDestination::class;
  protected $subDestinationsDataType = 'array';
  /**
   * @var bool
   */
  public $takeout;
  /**
   * @var string[]
   */
  public $types;
  /**
   * @var int
   */
  public $userRatingCount;
  /**
   * @var int
   */
  public $utcOffsetMinutes;
  protected $viewportType = GoogleGeoTypeViewport::class;
  protected $viewportDataType = '';
  /**
   * @var string
   */
  public $websiteUri;

  /**
   * @param GoogleMapsPlacesV1PlaceAccessibilityOptions
   */
  public function setAccessibilityOptions(GoogleMapsPlacesV1PlaceAccessibilityOptions $accessibilityOptions)
  {
    $this->accessibilityOptions = $accessibilityOptions;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceAccessibilityOptions
   */
  public function getAccessibilityOptions()
  {
    return $this->accessibilityOptions;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceAddressComponent[]
   */
  public function setAddressComponents($addressComponents)
  {
    $this->addressComponents = $addressComponents;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceAddressComponent[]
   */
  public function getAddressComponents()
  {
    return $this->addressComponents;
  }
  /**
   * @param GoogleMapsPlacesV1AddressDescriptor
   */
  public function setAddressDescriptor(GoogleMapsPlacesV1AddressDescriptor $addressDescriptor)
  {
    $this->addressDescriptor = $addressDescriptor;
  }
  /**
   * @return GoogleMapsPlacesV1AddressDescriptor
   */
  public function getAddressDescriptor()
  {
    return $this->addressDescriptor;
  }
  /**
   * @param string
   */
  public function setAdrFormatAddress($adrFormatAddress)
  {
    $this->adrFormatAddress = $adrFormatAddress;
  }
  /**
   * @return string
   */
  public function getAdrFormatAddress()
  {
    return $this->adrFormatAddress;
  }
  /**
   * @param bool
   */
  public function setAllowsDogs($allowsDogs)
  {
    $this->allowsDogs = $allowsDogs;
  }
  /**
   * @return bool
   */
  public function getAllowsDogs()
  {
    return $this->allowsDogs;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceAreaSummary
   */
  public function setAreaSummary(GoogleMapsPlacesV1PlaceAreaSummary $areaSummary)
  {
    $this->areaSummary = $areaSummary;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceAreaSummary
   */
  public function getAreaSummary()
  {
    return $this->areaSummary;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceAttribution[]
   */
  public function setAttributions($attributions)
  {
    $this->attributions = $attributions;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceAttribution[]
   */
  public function getAttributions()
  {
    return $this->attributions;
  }
  /**
   * @param string
   */
  public function setBusinessStatus($businessStatus)
  {
    $this->businessStatus = $businessStatus;
  }
  /**
   * @return string
   */
  public function getBusinessStatus()
  {
    return $this->businessStatus;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceContainingPlace[]
   */
  public function setContainingPlaces($containingPlaces)
  {
    $this->containingPlaces = $containingPlaces;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceContainingPlace[]
   */
  public function getContainingPlaces()
  {
    return $this->containingPlaces;
  }
  /**
   * @param bool
   */
  public function setCurbsidePickup($curbsidePickup)
  {
    $this->curbsidePickup = $curbsidePickup;
  }
  /**
   * @return bool
   */
  public function getCurbsidePickup()
  {
    return $this->curbsidePickup;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceOpeningHours
   */
  public function setCurrentOpeningHours(GoogleMapsPlacesV1PlaceOpeningHours $currentOpeningHours)
  {
    $this->currentOpeningHours = $currentOpeningHours;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceOpeningHours
   */
  public function getCurrentOpeningHours()
  {
    return $this->currentOpeningHours;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceOpeningHours[]
   */
  public function setCurrentSecondaryOpeningHours($currentSecondaryOpeningHours)
  {
    $this->currentSecondaryOpeningHours = $currentSecondaryOpeningHours;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceOpeningHours[]
   */
  public function getCurrentSecondaryOpeningHours()
  {
    return $this->currentSecondaryOpeningHours;
  }
  /**
   * @param bool
   */
  public function setDelivery($delivery)
  {
    $this->delivery = $delivery;
  }
  /**
   * @return bool
   */
  public function getDelivery()
  {
    return $this->delivery;
  }
  /**
   * @param bool
   */
  public function setDineIn($dineIn)
  {
    $this->dineIn = $dineIn;
  }
  /**
   * @return bool
   */
  public function getDineIn()
  {
    return $this->dineIn;
  }
  /**
   * @param GoogleTypeLocalizedText
   */
  public function setDisplayName(GoogleTypeLocalizedText $displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return GoogleTypeLocalizedText
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param GoogleTypeLocalizedText
   */
  public function setEditorialSummary(GoogleTypeLocalizedText $editorialSummary)
  {
    $this->editorialSummary = $editorialSummary;
  }
  /**
   * @return GoogleTypeLocalizedText
   */
  public function getEditorialSummary()
  {
    return $this->editorialSummary;
  }
  /**
   * @param GoogleMapsPlacesV1EVChargeOptions
   */
  public function setEvChargeOptions(GoogleMapsPlacesV1EVChargeOptions $evChargeOptions)
  {
    $this->evChargeOptions = $evChargeOptions;
  }
  /**
   * @return GoogleMapsPlacesV1EVChargeOptions
   */
  public function getEvChargeOptions()
  {
    return $this->evChargeOptions;
  }
  /**
   * @param string
   */
  public function setFormattedAddress($formattedAddress)
  {
    $this->formattedAddress = $formattedAddress;
  }
  /**
   * @return string
   */
  public function getFormattedAddress()
  {
    return $this->formattedAddress;
  }
  /**
   * @param GoogleMapsPlacesV1FuelOptions
   */
  public function setFuelOptions(GoogleMapsPlacesV1FuelOptions $fuelOptions)
  {
    $this->fuelOptions = $fuelOptions;
  }
  /**
   * @return GoogleMapsPlacesV1FuelOptions
   */
  public function getFuelOptions()
  {
    return $this->fuelOptions;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceGenerativeSummary
   */
  public function setGenerativeSummary(GoogleMapsPlacesV1PlaceGenerativeSummary $generativeSummary)
  {
    $this->generativeSummary = $generativeSummary;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceGenerativeSummary
   */
  public function getGenerativeSummary()
  {
    return $this->generativeSummary;
  }
  /**
   * @param bool
   */
  public function setGoodForChildren($goodForChildren)
  {
    $this->goodForChildren = $goodForChildren;
  }
  /**
   * @return bool
   */
  public function getGoodForChildren()
  {
    return $this->goodForChildren;
  }
  /**
   * @param bool
   */
  public function setGoodForGroups($goodForGroups)
  {
    $this->goodForGroups = $goodForGroups;
  }
  /**
   * @return bool
   */
  public function getGoodForGroups()
  {
    return $this->goodForGroups;
  }
  /**
   * @param bool
   */
  public function setGoodForWatchingSports($goodForWatchingSports)
  {
    $this->goodForWatchingSports = $goodForWatchingSports;
  }
  /**
   * @return bool
   */
  public function getGoodForWatchingSports()
  {
    return $this->goodForWatchingSports;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceGoogleMapsLinks
   */
  public function setGoogleMapsLinks(GoogleMapsPlacesV1PlaceGoogleMapsLinks $googleMapsLinks)
  {
    $this->googleMapsLinks = $googleMapsLinks;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceGoogleMapsLinks
   */
  public function getGoogleMapsLinks()
  {
    return $this->googleMapsLinks;
  }
  /**
   * @param string
   */
  public function setGoogleMapsUri($googleMapsUri)
  {
    $this->googleMapsUri = $googleMapsUri;
  }
  /**
   * @return string
   */
  public function getGoogleMapsUri()
  {
    return $this->googleMapsUri;
  }
  /**
   * @param string
   */
  public function setIconBackgroundColor($iconBackgroundColor)
  {
    $this->iconBackgroundColor = $iconBackgroundColor;
  }
  /**
   * @return string
   */
  public function getIconBackgroundColor()
  {
    return $this->iconBackgroundColor;
  }
  /**
   * @param string
   */
  public function setIconMaskBaseUri($iconMaskBaseUri)
  {
    $this->iconMaskBaseUri = $iconMaskBaseUri;
  }
  /**
   * @return string
   */
  public function getIconMaskBaseUri()
  {
    return $this->iconMaskBaseUri;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param string
   */
  public function setInternationalPhoneNumber($internationalPhoneNumber)
  {
    $this->internationalPhoneNumber = $internationalPhoneNumber;
  }
  /**
   * @return string
   */
  public function getInternationalPhoneNumber()
  {
    return $this->internationalPhoneNumber;
  }
  /**
   * @param bool
   */
  public function setLiveMusic($liveMusic)
  {
    $this->liveMusic = $liveMusic;
  }
  /**
   * @return bool
   */
  public function getLiveMusic()
  {
    return $this->liveMusic;
  }
  /**
   * @param GoogleTypeLatLng
   */
  public function setLocation(GoogleTypeLatLng $location)
  {
    $this->location = $location;
  }
  /**
   * @return GoogleTypeLatLng
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param bool
   */
  public function setMenuForChildren($menuForChildren)
  {
    $this->menuForChildren = $menuForChildren;
  }
  /**
   * @return bool
   */
  public function getMenuForChildren()
  {
    return $this->menuForChildren;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setNationalPhoneNumber($nationalPhoneNumber)
  {
    $this->nationalPhoneNumber = $nationalPhoneNumber;
  }
  /**
   * @return string
   */
  public function getNationalPhoneNumber()
  {
    return $this->nationalPhoneNumber;
  }
  /**
   * @param bool
   */
  public function setOutdoorSeating($outdoorSeating)
  {
    $this->outdoorSeating = $outdoorSeating;
  }
  /**
   * @return bool
   */
  public function getOutdoorSeating()
  {
    return $this->outdoorSeating;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceParkingOptions
   */
  public function setParkingOptions(GoogleMapsPlacesV1PlaceParkingOptions $parkingOptions)
  {
    $this->parkingOptions = $parkingOptions;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceParkingOptions
   */
  public function getParkingOptions()
  {
    return $this->parkingOptions;
  }
  /**
   * @param GoogleMapsPlacesV1PlacePaymentOptions
   */
  public function setPaymentOptions(GoogleMapsPlacesV1PlacePaymentOptions $paymentOptions)
  {
    $this->paymentOptions = $paymentOptions;
  }
  /**
   * @return GoogleMapsPlacesV1PlacePaymentOptions
   */
  public function getPaymentOptions()
  {
    return $this->paymentOptions;
  }
  /**
   * @param GoogleMapsPlacesV1Photo[]
   */
  public function setPhotos($photos)
  {
    $this->photos = $photos;
  }
  /**
   * @return GoogleMapsPlacesV1Photo[]
   */
  public function getPhotos()
  {
    return $this->photos;
  }
  /**
   * @param GoogleMapsPlacesV1PlacePlusCode
   */
  public function setPlusCode(GoogleMapsPlacesV1PlacePlusCode $plusCode)
  {
    $this->plusCode = $plusCode;
  }
  /**
   * @return GoogleMapsPlacesV1PlacePlusCode
   */
  public function getPlusCode()
  {
    return $this->plusCode;
  }
  /**
   * @param string
   */
  public function setPriceLevel($priceLevel)
  {
    $this->priceLevel = $priceLevel;
  }
  /**
   * @return string
   */
  public function getPriceLevel()
  {
    return $this->priceLevel;
  }
  /**
   * @param GoogleMapsPlacesV1PriceRange
   */
  public function setPriceRange(GoogleMapsPlacesV1PriceRange $priceRange)
  {
    $this->priceRange = $priceRange;
  }
  /**
   * @return GoogleMapsPlacesV1PriceRange
   */
  public function getPriceRange()
  {
    return $this->priceRange;
  }
  /**
   * @param string
   */
  public function setPrimaryType($primaryType)
  {
    $this->primaryType = $primaryType;
  }
  /**
   * @return string
   */
  public function getPrimaryType()
  {
    return $this->primaryType;
  }
  /**
   * @param GoogleTypeLocalizedText
   */
  public function setPrimaryTypeDisplayName(GoogleTypeLocalizedText $primaryTypeDisplayName)
  {
    $this->primaryTypeDisplayName = $primaryTypeDisplayName;
  }
  /**
   * @return GoogleTypeLocalizedText
   */
  public function getPrimaryTypeDisplayName()
  {
    return $this->primaryTypeDisplayName;
  }
  /**
   * @param bool
   */
  public function setPureServiceAreaBusiness($pureServiceAreaBusiness)
  {
    $this->pureServiceAreaBusiness = $pureServiceAreaBusiness;
  }
  /**
   * @return bool
   */
  public function getPureServiceAreaBusiness()
  {
    return $this->pureServiceAreaBusiness;
  }
  public function setRating($rating)
  {
    $this->rating = $rating;
  }
  public function getRating()
  {
    return $this->rating;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceOpeningHours
   */
  public function setRegularOpeningHours(GoogleMapsPlacesV1PlaceOpeningHours $regularOpeningHours)
  {
    $this->regularOpeningHours = $regularOpeningHours;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceOpeningHours
   */
  public function getRegularOpeningHours()
  {
    return $this->regularOpeningHours;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceOpeningHours[]
   */
  public function setRegularSecondaryOpeningHours($regularSecondaryOpeningHours)
  {
    $this->regularSecondaryOpeningHours = $regularSecondaryOpeningHours;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceOpeningHours[]
   */
  public function getRegularSecondaryOpeningHours()
  {
    return $this->regularSecondaryOpeningHours;
  }
  /**
   * @param bool
   */
  public function setReservable($reservable)
  {
    $this->reservable = $reservable;
  }
  /**
   * @return bool
   */
  public function getReservable()
  {
    return $this->reservable;
  }
  /**
   * @param bool
   */
  public function setRestroom($restroom)
  {
    $this->restroom = $restroom;
  }
  /**
   * @return bool
   */
  public function getRestroom()
  {
    return $this->restroom;
  }
  /**
   * @param GoogleMapsPlacesV1Review[]
   */
  public function setReviews($reviews)
  {
    $this->reviews = $reviews;
  }
  /**
   * @return GoogleMapsPlacesV1Review[]
   */
  public function getReviews()
  {
    return $this->reviews;
  }
  /**
   * @param bool
   */
  public function setServesBeer($servesBeer)
  {
    $this->servesBeer = $servesBeer;
  }
  /**
   * @return bool
   */
  public function getServesBeer()
  {
    return $this->servesBeer;
  }
  /**
   * @param bool
   */
  public function setServesBreakfast($servesBreakfast)
  {
    $this->servesBreakfast = $servesBreakfast;
  }
  /**
   * @return bool
   */
  public function getServesBreakfast()
  {
    return $this->servesBreakfast;
  }
  /**
   * @param bool
   */
  public function setServesBrunch($servesBrunch)
  {
    $this->servesBrunch = $servesBrunch;
  }
  /**
   * @return bool
   */
  public function getServesBrunch()
  {
    return $this->servesBrunch;
  }
  /**
   * @param bool
   */
  public function setServesCocktails($servesCocktails)
  {
    $this->servesCocktails = $servesCocktails;
  }
  /**
   * @return bool
   */
  public function getServesCocktails()
  {
    return $this->servesCocktails;
  }
  /**
   * @param bool
   */
  public function setServesCoffee($servesCoffee)
  {
    $this->servesCoffee = $servesCoffee;
  }
  /**
   * @return bool
   */
  public function getServesCoffee()
  {
    return $this->servesCoffee;
  }
  /**
   * @param bool
   */
  public function setServesDessert($servesDessert)
  {
    $this->servesDessert = $servesDessert;
  }
  /**
   * @return bool
   */
  public function getServesDessert()
  {
    return $this->servesDessert;
  }
  /**
   * @param bool
   */
  public function setServesDinner($servesDinner)
  {
    $this->servesDinner = $servesDinner;
  }
  /**
   * @return bool
   */
  public function getServesDinner()
  {
    return $this->servesDinner;
  }
  /**
   * @param bool
   */
  public function setServesLunch($servesLunch)
  {
    $this->servesLunch = $servesLunch;
  }
  /**
   * @return bool
   */
  public function getServesLunch()
  {
    return $this->servesLunch;
  }
  /**
   * @param bool
   */
  public function setServesVegetarianFood($servesVegetarianFood)
  {
    $this->servesVegetarianFood = $servesVegetarianFood;
  }
  /**
   * @return bool
   */
  public function getServesVegetarianFood()
  {
    return $this->servesVegetarianFood;
  }
  /**
   * @param bool
   */
  public function setServesWine($servesWine)
  {
    $this->servesWine = $servesWine;
  }
  /**
   * @return bool
   */
  public function getServesWine()
  {
    return $this->servesWine;
  }
  /**
   * @param string
   */
  public function setShortFormattedAddress($shortFormattedAddress)
  {
    $this->shortFormattedAddress = $shortFormattedAddress;
  }
  /**
   * @return string
   */
  public function getShortFormattedAddress()
  {
    return $this->shortFormattedAddress;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceSubDestination[]
   */
  public function setSubDestinations($subDestinations)
  {
    $this->subDestinations = $subDestinations;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceSubDestination[]
   */
  public function getSubDestinations()
  {
    return $this->subDestinations;
  }
  /**
   * @param bool
   */
  public function setTakeout($takeout)
  {
    $this->takeout = $takeout;
  }
  /**
   * @return bool
   */
  public function getTakeout()
  {
    return $this->takeout;
  }
  /**
   * @param string[]
   */
  public function setTypes($types)
  {
    $this->types = $types;
  }
  /**
   * @return string[]
   */
  public function getTypes()
  {
    return $this->types;
  }
  /**
   * @param int
   */
  public function setUserRatingCount($userRatingCount)
  {
    $this->userRatingCount = $userRatingCount;
  }
  /**
   * @return int
   */
  public function getUserRatingCount()
  {
    return $this->userRatingCount;
  }
  /**
   * @param int
   */
  public function setUtcOffsetMinutes($utcOffsetMinutes)
  {
    $this->utcOffsetMinutes = $utcOffsetMinutes;
  }
  /**
   * @return int
   */
  public function getUtcOffsetMinutes()
  {
    return $this->utcOffsetMinutes;
  }
  /**
   * @param GoogleGeoTypeViewport
   */
  public function setViewport(GoogleGeoTypeViewport $viewport)
  {
    $this->viewport = $viewport;
  }
  /**
   * @return GoogleGeoTypeViewport
   */
  public function getViewport()
  {
    return $this->viewport;
  }
  /**
   * @param string
   */
  public function setWebsiteUri($websiteUri)
  {
    $this->websiteUri = $websiteUri;
  }
  /**
   * @return string
   */
  public function getWebsiteUri()
  {
    return $this->websiteUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1Place::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1Place');
