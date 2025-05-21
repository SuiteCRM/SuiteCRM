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

namespace Google\Service\BlockchainNodeEngine;

class EthereumEndpoints extends \Google\Model
{
  /**
   * @var string
   */
  public $beaconApiEndpoint;
  /**
   * @var string
   */
  public $beaconPrometheusMetricsApiEndpoint;
  /**
   * @var string
   */
  public $executionClientPrometheusMetricsApiEndpoint;

  /**
   * @param string
   */
  public function setBeaconApiEndpoint($beaconApiEndpoint)
  {
    $this->beaconApiEndpoint = $beaconApiEndpoint;
  }
  /**
   * @return string
   */
  public function getBeaconApiEndpoint()
  {
    return $this->beaconApiEndpoint;
  }
  /**
   * @param string
   */
  public function setBeaconPrometheusMetricsApiEndpoint($beaconPrometheusMetricsApiEndpoint)
  {
    $this->beaconPrometheusMetricsApiEndpoint = $beaconPrometheusMetricsApiEndpoint;
  }
  /**
   * @return string
   */
  public function getBeaconPrometheusMetricsApiEndpoint()
  {
    return $this->beaconPrometheusMetricsApiEndpoint;
  }
  /**
   * @param string
   */
  public function setExecutionClientPrometheusMetricsApiEndpoint($executionClientPrometheusMetricsApiEndpoint)
  {
    $this->executionClientPrometheusMetricsApiEndpoint = $executionClientPrometheusMetricsApiEndpoint;
  }
  /**
   * @return string
   */
  public function getExecutionClientPrometheusMetricsApiEndpoint()
  {
    return $this->executionClientPrometheusMetricsApiEndpoint;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EthereumEndpoints::class, 'Google_Service_BlockchainNodeEngine_EthereumEndpoints');
