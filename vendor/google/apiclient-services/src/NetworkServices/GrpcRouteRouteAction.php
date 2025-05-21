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

class GrpcRouteRouteAction extends \Google\Collection
{
  protected $collection_key = 'destinations';
  protected $destinationsType = GrpcRouteDestination::class;
  protected $destinationsDataType = 'array';
  protected $faultInjectionPolicyType = GrpcRouteFaultInjectionPolicy::class;
  protected $faultInjectionPolicyDataType = '';
  /**
   * @var string
   */
  public $idleTimeout;
  protected $retryPolicyType = GrpcRouteRetryPolicy::class;
  protected $retryPolicyDataType = '';
  protected $statefulSessionAffinityType = GrpcRouteStatefulSessionAffinityPolicy::class;
  protected $statefulSessionAffinityDataType = '';
  /**
   * @var string
   */
  public $timeout;

  /**
   * @param GrpcRouteDestination[]
   */
  public function setDestinations($destinations)
  {
    $this->destinations = $destinations;
  }
  /**
   * @return GrpcRouteDestination[]
   */
  public function getDestinations()
  {
    return $this->destinations;
  }
  /**
   * @param GrpcRouteFaultInjectionPolicy
   */
  public function setFaultInjectionPolicy(GrpcRouteFaultInjectionPolicy $faultInjectionPolicy)
  {
    $this->faultInjectionPolicy = $faultInjectionPolicy;
  }
  /**
   * @return GrpcRouteFaultInjectionPolicy
   */
  public function getFaultInjectionPolicy()
  {
    return $this->faultInjectionPolicy;
  }
  /**
   * @param string
   */
  public function setIdleTimeout($idleTimeout)
  {
    $this->idleTimeout = $idleTimeout;
  }
  /**
   * @return string
   */
  public function getIdleTimeout()
  {
    return $this->idleTimeout;
  }
  /**
   * @param GrpcRouteRetryPolicy
   */
  public function setRetryPolicy(GrpcRouteRetryPolicy $retryPolicy)
  {
    $this->retryPolicy = $retryPolicy;
  }
  /**
   * @return GrpcRouteRetryPolicy
   */
  public function getRetryPolicy()
  {
    return $this->retryPolicy;
  }
  /**
   * @param GrpcRouteStatefulSessionAffinityPolicy
   */
  public function setStatefulSessionAffinity(GrpcRouteStatefulSessionAffinityPolicy $statefulSessionAffinity)
  {
    $this->statefulSessionAffinity = $statefulSessionAffinity;
  }
  /**
   * @return GrpcRouteStatefulSessionAffinityPolicy
   */
  public function getStatefulSessionAffinity()
  {
    return $this->statefulSessionAffinity;
  }
  /**
   * @param string
   */
  public function setTimeout($timeout)
  {
    $this->timeout = $timeout;
  }
  /**
   * @return string
   */
  public function getTimeout()
  {
    return $this->timeout;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GrpcRouteRouteAction::class, 'Google_Service_NetworkServices_GrpcRouteRouteAction');
