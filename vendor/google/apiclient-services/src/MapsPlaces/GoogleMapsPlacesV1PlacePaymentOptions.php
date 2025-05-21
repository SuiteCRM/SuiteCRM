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

class GoogleMapsPlacesV1PlacePaymentOptions extends \Google\Model
{
  /**
   * @var bool
   */
  public $acceptsCashOnly;
  /**
   * @var bool
   */
  public $acceptsCreditCards;
  /**
   * @var bool
   */
  public $acceptsDebitCards;
  /**
   * @var bool
   */
  public $acceptsNfc;

  /**
   * @param bool
   */
  public function setAcceptsCashOnly($acceptsCashOnly)
  {
    $this->acceptsCashOnly = $acceptsCashOnly;
  }
  /**
   * @return bool
   */
  public function getAcceptsCashOnly()
  {
    return $this->acceptsCashOnly;
  }
  /**
   * @param bool
   */
  public function setAcceptsCreditCards($acceptsCreditCards)
  {
    $this->acceptsCreditCards = $acceptsCreditCards;
  }
  /**
   * @return bool
   */
  public function getAcceptsCreditCards()
  {
    return $this->acceptsCreditCards;
  }
  /**
   * @param bool
   */
  public function setAcceptsDebitCards($acceptsDebitCards)
  {
    $this->acceptsDebitCards = $acceptsDebitCards;
  }
  /**
   * @return bool
   */
  public function getAcceptsDebitCards()
  {
    return $this->acceptsDebitCards;
  }
  /**
   * @param bool
   */
  public function setAcceptsNfc($acceptsNfc)
  {
    $this->acceptsNfc = $acceptsNfc;
  }
  /**
   * @return bool
   */
  public function getAcceptsNfc()
  {
    return $this->acceptsNfc;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1PlacePaymentOptions::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1PlacePaymentOptions');
