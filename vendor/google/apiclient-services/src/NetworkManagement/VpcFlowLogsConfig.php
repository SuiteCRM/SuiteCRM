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

namespace Google\Service\NetworkManagement;

class VpcFlowLogsConfig extends \Google\Collection
{
  protected $collection_key = 'metadataFields';
  /**
   * @var string
   */
  public $aggregationInterval;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $filterExpr;
  /**
   * @var float
   */
  public $flowSampling;
  /**
   * @var string
   */
  public $interconnectAttachment;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $metadata;
  /**
   * @var string[]
   */
  public $metadataFields;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $targetResourceState;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string
   */
  public $vpnTunnel;

  /**
   * @param string
   */
  public function setAggregationInterval($aggregationInterval)
  {
    $this->aggregationInterval = $aggregationInterval;
  }
  /**
   * @return string
   */
  public function getAggregationInterval()
  {
    return $this->aggregationInterval;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setFilterExpr($filterExpr)
  {
    $this->filterExpr = $filterExpr;
  }
  /**
   * @return string
   */
  public function getFilterExpr()
  {
    return $this->filterExpr;
  }
  /**
   * @param float
   */
  public function setFlowSampling($flowSampling)
  {
    $this->flowSampling = $flowSampling;
  }
  /**
   * @return float
   */
  public function getFlowSampling()
  {
    return $this->flowSampling;
  }
  /**
   * @param string
   */
  public function setInterconnectAttachment($interconnectAttachment)
  {
    $this->interconnectAttachment = $interconnectAttachment;
  }
  /**
   * @return string
   */
  public function getInterconnectAttachment()
  {
    return $this->interconnectAttachment;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param string
   */
  public function setMetadata($metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return string
   */
  public function getMetadata()
  {
    return $this->metadata;
  }
  /**
   * @param string[]
   */
  public function setMetadataFields($metadataFields)
  {
    $this->metadataFields = $metadataFields;
  }
  /**
   * @return string[]
   */
  public function getMetadataFields()
  {
    return $this->metadataFields;
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
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setTargetResourceState($targetResourceState)
  {
    $this->targetResourceState = $targetResourceState;
  }
  /**
   * @return string
   */
  public function getTargetResourceState()
  {
    return $this->targetResourceState;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
  /**
   * @param string
   */
  public function setVpnTunnel($vpnTunnel)
  {
    $this->vpnTunnel = $vpnTunnel;
  }
  /**
   * @return string
   */
  public function getVpnTunnel()
  {
    return $this->vpnTunnel;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VpcFlowLogsConfig::class, 'Google_Service_NetworkManagement_VpcFlowLogsConfig');
