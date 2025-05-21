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

namespace Google\Service\Dataproc;

class AuxiliaryNodeGroup extends \Google\Model
{
  protected $nodeGroupType = NodeGroup::class;
  protected $nodeGroupDataType = '';
  /**
   * @var string
   */
  public $nodeGroupId;

  /**
   * @param NodeGroup
   */
  public function setNodeGroup(NodeGroup $nodeGroup)
  {
    $this->nodeGroup = $nodeGroup;
  }
  /**
   * @return NodeGroup
   */
  public function getNodeGroup()
  {
    return $this->nodeGroup;
  }
  /**
   * @param string
   */
  public function setNodeGroupId($nodeGroupId)
  {
    $this->nodeGroupId = $nodeGroupId;
  }
  /**
   * @return string
   */
  public function getNodeGroupId()
  {
    return $this->nodeGroupId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AuxiliaryNodeGroup::class, 'Google_Service_Dataproc_AuxiliaryNodeGroup');
