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

namespace Google\Service\Compute;

class BgpRoute extends \Google\Collection
{
  protected $collection_key = 'communities';
  protected $asPathsType = BgpRouteAsPath::class;
  protected $asPathsDataType = 'array';
  /**
   * @var string[]
   */
  public $communities;
  protected $destinationType = BgpRouteNetworkLayerReachabilityInformation::class;
  protected $destinationDataType = '';
  /**
   * @var string
   */
  public $med;
  /**
   * @var string
   */
  public $origin;

  /**
   * @param BgpRouteAsPath[]
   */
  public function setAsPaths($asPaths)
  {
    $this->asPaths = $asPaths;
  }
  /**
   * @return BgpRouteAsPath[]
   */
  public function getAsPaths()
  {
    return $this->asPaths;
  }
  /**
   * @param string[]
   */
  public function setCommunities($communities)
  {
    $this->communities = $communities;
  }
  /**
   * @return string[]
   */
  public function getCommunities()
  {
    return $this->communities;
  }
  /**
   * @param BgpRouteNetworkLayerReachabilityInformation
   */
  public function setDestination(BgpRouteNetworkLayerReachabilityInformation $destination)
  {
    $this->destination = $destination;
  }
  /**
   * @return BgpRouteNetworkLayerReachabilityInformation
   */
  public function getDestination()
  {
    return $this->destination;
  }
  /**
   * @param string
   */
  public function setMed($med)
  {
    $this->med = $med;
  }
  /**
   * @return string
   */
  public function getMed()
  {
    return $this->med;
  }
  /**
   * @param string
   */
  public function setOrigin($origin)
  {
    $this->origin = $origin;
  }
  /**
   * @return string
   */
  public function getOrigin()
  {
    return $this->origin;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BgpRoute::class, 'Google_Service_Compute_BgpRoute');
