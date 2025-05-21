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

class BlockchainNode extends \Google\Model
{
  /**
   * @var string
   */
  public $blockchainType;
  protected $connectionInfoType = ConnectionInfo::class;
  protected $connectionInfoDataType = '';
  /**
   * @var string
   */
  public $createTime;
  protected $ethereumDetailsType = EthereumDetails::class;
  protected $ethereumDetailsDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var bool
   */
  public $privateServiceConnectEnabled;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setBlockchainType($blockchainType)
  {
    $this->blockchainType = $blockchainType;
  }
  /**
   * @return string
   */
  public function getBlockchainType()
  {
    return $this->blockchainType;
  }
  /**
   * @param ConnectionInfo
   */
  public function setConnectionInfo(ConnectionInfo $connectionInfo)
  {
    $this->connectionInfo = $connectionInfo;
  }
  /**
   * @return ConnectionInfo
   */
  public function getConnectionInfo()
  {
    return $this->connectionInfo;
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
   * @param EthereumDetails
   */
  public function setEthereumDetails(EthereumDetails $ethereumDetails)
  {
    $this->ethereumDetails = $ethereumDetails;
  }
  /**
   * @return EthereumDetails
   */
  public function getEthereumDetails()
  {
    return $this->ethereumDetails;
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
   * @param bool
   */
  public function setPrivateServiceConnectEnabled($privateServiceConnectEnabled)
  {
    $this->privateServiceConnectEnabled = $privateServiceConnectEnabled;
  }
  /**
   * @return bool
   */
  public function getPrivateServiceConnectEnabled()
  {
    return $this->privateServiceConnectEnabled;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BlockchainNode::class, 'Google_Service_BlockchainNodeEngine_BlockchainNode');
