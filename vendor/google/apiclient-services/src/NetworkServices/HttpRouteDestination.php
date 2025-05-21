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

namespace Google\Service\NetworkServices;

class HttpRouteDestination extends \Google\Model
{
  protected $requestHeaderModifierType = HttpRouteHeaderModifier::class;
  protected $requestHeaderModifierDataType = '';
  protected $responseHeaderModifierType = HttpRouteHeaderModifier::class;
  protected $responseHeaderModifierDataType = '';
  /**
   * @var string
   */
  public $serviceName;
  /**
   * @var int
   */
  public $weight;

  /**
   * @param HttpRouteHeaderModifier
   */
  public function setRequestHeaderModifier(HttpRouteHeaderModifier $requestHeaderModifier)
  {
    $this->requestHeaderModifier = $requestHeaderModifier;
  }
  /**
   * @return HttpRouteHeaderModifier
   */
  public function getRequestHeaderModifier()
  {
    return $this->requestHeaderModifier;
  }
  /**
   * @param HttpRouteHeaderModifier
   */
  public function setResponseHeaderModifier(HttpRouteHeaderModifier $responseHeaderModifier)
  {
    $this->responseHeaderModifier = $responseHeaderModifier;
  }
  /**
   * @return HttpRouteHeaderModifier
   */
  public function getResponseHeaderModifier()
  {
    return $this->responseHeaderModifier;
  }
  /**
   * @param string
   */
  public function setServiceName($serviceName)
  {
    $this->serviceName = $serviceName;
  }
  /**
   * @return string
   */
  public function getServiceName()
  {
    return $this->serviceName;
  }
  /**
   * @param int
   */
  public function setWeight($weight)
  {
    $this->weight = $weight;
  }
  /**
   * @return int
   */
  public function getWeight()
  {
    return $this->weight;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(HttpRouteDestination::class, 'Google_Service_NetworkServices_HttpRouteDestination');
