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

class BgpRouteNetworkLayerReachabilityInformation extends \Google\Model
{
  /**
   * @var string
   */
  public $pathId;
  /**
   * @var string
   */
  public $prefix;

  /**
   * @param string
   */
  public function setPathId($pathId)
  {
    $this->pathId = $pathId;
  }
  /**
   * @return string
   */
  public function getPathId()
  {
    return $this->pathId;
  }
  /**
   * @param string
   */
  public function setPrefix($prefix)
  {
    $this->prefix = $prefix;
  }
  /**
   * @return string
   */
  public function getPrefix()
  {
    return $this->prefix;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BgpRouteNetworkLayerReachabilityInformation::class, 'Google_Service_Compute_BgpRouteNetworkLayerReachabilityInformation');
