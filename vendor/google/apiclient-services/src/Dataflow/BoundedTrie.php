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

namespace Google\Service\Dataflow;

class BoundedTrie extends \Google\Collection
{
  protected $collection_key = 'singleton';
  /**
   * @var int
   */
  public $bound;
  protected $rootType = BoundedTrieNode::class;
  protected $rootDataType = '';
  /**
   * @var string[]
   */
  public $singleton;

  /**
   * @param int
   */
  public function setBound($bound)
  {
    $this->bound = $bound;
  }
  /**
   * @return int
   */
  public function getBound()
  {
    return $this->bound;
  }
  /**
   * @param BoundedTrieNode
   */
  public function setRoot(BoundedTrieNode $root)
  {
    $this->root = $root;
  }
  /**
   * @return BoundedTrieNode
   */
  public function getRoot()
  {
    return $this->root;
  }
  /**
   * @param string[]
   */
  public function setSingleton($singleton)
  {
    $this->singleton = $singleton;
  }
  /**
   * @return string[]
   */
  public function getSingleton()
  {
    return $this->singleton;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BoundedTrie::class, 'Google_Service_Dataflow_BoundedTrie');
