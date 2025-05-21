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

namespace Google\Service\CloudDeploy;

class RouteDestinations extends \Google\Collection
{
  protected $collection_key = 'destinationIds';
  /**
   * @var string[]
   */
  public $destinationIds;
  /**
   * @var bool
   */
  public $propagateService;

  /**
   * @param string[]
   */
  public function setDestinationIds($destinationIds)
  {
    $this->destinationIds = $destinationIds;
  }
  /**
   * @return string[]
   */
  public function getDestinationIds()
  {
    return $this->destinationIds;
  }
  /**
   * @param bool
   */
  public function setPropagateService($propagateService)
  {
    $this->propagateService = $propagateService;
  }
  /**
   * @return bool
   */
  public function getPropagateService()
  {
    return $this->propagateService;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RouteDestinations::class, 'Google_Service_CloudDeploy_RouteDestinations');
