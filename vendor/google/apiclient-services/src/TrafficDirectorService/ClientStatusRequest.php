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

namespace Google\Service\TrafficDirectorService;

class ClientStatusRequest extends \Google\Collection
{
  protected $collection_key = 'nodeMatchers';
  /**
   * @var bool
   */
  public $excludeResourceContents;
  protected $nodeType = Node::class;
  protected $nodeDataType = '';
  protected $nodeMatchersType = NodeMatcher::class;
  protected $nodeMatchersDataType = 'array';

  /**
   * @param bool
   */
  public function setExcludeResourceContents($excludeResourceContents)
  {
    $this->excludeResourceContents = $excludeResourceContents;
  }
  /**
   * @return bool
   */
  public function getExcludeResourceContents()
  {
    return $this->excludeResourceContents;
  }
  /**
   * @param Node
   */
  public function setNode(Node $node)
  {
    $this->node = $node;
  }
  /**
   * @return Node
   */
  public function getNode()
  {
    return $this->node;
  }
  /**
   * @param NodeMatcher[]
   */
  public function setNodeMatchers($nodeMatchers)
  {
    $this->nodeMatchers = $nodeMatchers;
  }
  /**
   * @return NodeMatcher[]
   */
  public function getNodeMatchers()
  {
    return $this->nodeMatchers;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ClientStatusRequest::class, 'Google_Service_TrafficDirectorService_ClientStatusRequest');
